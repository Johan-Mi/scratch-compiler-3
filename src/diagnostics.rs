use codemap::Span;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};

#[derive(Default)]
pub struct Diagnostics(Vec<Diagnostic>);

impl Diagnostics {
    pub fn show(mut self, code_map: &codemap::CodeMap) {
        if self.0.is_empty() {
            return;
        }

        match self.0.iter().filter(|it| it.level == Level::Error).count() {
            0 => {}
            1 => self.note("1 error was generated during compilation", []),
            total_errors => self.note(
                format!("{total_errors} errors were generated during compilation"),
                [],
            ),
        }

        Emitter::stderr(ColorConfig::Auto, Some(code_map)).emit(&self.0);
    }

    pub fn error(&mut self, message: impl Into<String>, labels: impl Into<Vec<SpanLabel>>) {
        self.0.push(Diagnostic {
            level: Level::Error,
            message: message.into(),
            code: None,
            spans: labels.into(),
        });
    }

    pub fn note(&mut self, message: impl Into<String>, labels: impl Into<Vec<SpanLabel>>) {
        self.0.push(Diagnostic {
            level: Level::Note,
            message: message.into(),
            code: None,
            spans: labels.into(),
        });
    }

    pub fn warning(&mut self, message: impl Into<String>, labels: impl Into<Vec<SpanLabel>>) {
        self.0.push(Diagnostic {
            level: Level::Warning,
            message: message.into(),
            code: None,
            spans: labels.into(),
        });
    }

    pub fn help(&mut self, message: impl Into<String>, labels: impl Into<Vec<SpanLabel>>) {
        self.0.push(Diagnostic {
            level: Level::Help,
            message: message.into(),
            code: None,
            spans: labels.into(),
        });
    }

    pub fn have_errors(&self) -> bool {
        self.0.iter().any(|it| it.level == Level::Error)
    }
}

pub fn primary(span: Span, text: impl Into<String>) -> SpanLabel {
    SpanLabel {
        span,
        label: Some(text.into()),
        style: SpanStyle::Primary,
    }
}

pub fn secondary(span: Span, text: impl Into<String>) -> SpanLabel {
    SpanLabel {
        span,
        label: Some(text.into()),
        style: SpanStyle::Secondary,
    }
}

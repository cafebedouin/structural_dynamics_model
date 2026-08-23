% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right as Fundamental Constitutional Mechanism (Magistrate Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   The remonstrance arrangement of the French Old Regime: no royal edict —
 *   above all no fiscal edict — took legal force within a sovereign court's
 *   jurisdiction until that court enrolled it, and the courts (above all the
 *   Parlement of Paris) claimed the right to remonstrate formally against an
 *   edict before enrolling it. The crown could force enrollment (lit de
 *   justice), exile magistrates, or abolish and replace the courts (Maupeou,
 *   1771); the courts could delay, amend, and publicly contest royal
 *   legislation while claiming to act as guardians of the kingdom's
 *   fundamental laws. This file instantiates the magistrate_reading of the
 *   remonstrance_authority kernel — the remonstrance right as fundamental
 *   constitutional mechanism preserving ancient liberties against arbitrary
 *   innovation — as one clean, epsilon-invariant constraint. The referent of
 *   epsilon is the standing registration-and-remonstrance arrangement as it
 *   operated from the vingtieme crises of the 1750s to the terminal
 *   confrontation of 1787-88 (interval T0=1750 to T38=1788), never the
 *   reading's endorsed alternative. The claimed type (mountain) is the
 *   reading's own framing — the magistracy presented the right as immemorial
 *   fundamental law, beyond royal construction — authored independently of
 *   the metrics, which describe the arrangement's actual operation:
 *   substantially extractive on its fiscal-reform face, per the manifest's
 *   structural delta (high epsilon for fiscal reform edicts; Parlements
 *   victimized when overridden; a beneficiary class of tax-exempt
 *   magistracy). Declaring beneficiaries on a mountain claim is deliberate
 *   false-summit authoring: the tax-exempt magistracy is an identifiable
 *   beneficiary class of a constraint presented as ancient and necessary, and
 *   the schema-required omega documents the natural-law-versus-construction
 *   ambiguity. The sibling reading (crown_reading: the right as illegitimate
 *   minoritarian veto protecting particularist privileges) is a separate
 *   constraint file with its own epsilon over the same referent, linked
 *   through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - parlements_of_france: agenda-setting court and override-phase victim (institutional power / identity-locked exit) — administers the registration-and-remonstrance machinery, bears exile and suppression when the crown overrides
 *   - tax_exempt_magistracy: primary beneficiary (institutional / identity-locked) — the robe nobility whose tax exemptions are defended by the veto its own members administer; the same men as the courts in their class capacity
 *   - crown_fiscal_authority: payer with a secondary alibi benefit (institutional / constrained) — proposes fiscal edicts, pays override costs, collects the dividend of being seen to be blocked
 *   - unprivileged_taxpayers: silent payers (powerless / trapped) — taille-paying commoners who bear the burden distribution the veto freezes; no seat in the ritual
 *   - privileged_tax_orders: incidental beneficiaries (organized / identity-locked) — clergy and sword nobility whose exemptions the courts' remonstrances defend
 *   - representative_assembly_advocates: excluded voice (moderate / trapped) — reformers who locate consent in a representative assembly the arrangement has no channel for
 *   - constitutional_historians: analytical observer (analytical / analytical) — reconstructs the full structure from outside every seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.78).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.78).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, mountain).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right as Fundamental Constitutional Mechanism (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).
domain_priors:emerges_naturally(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '88131356-acba-429e-8a45-93e92ea505f4').
narrative_ontology:cs_kernel_codification('88131356-acba-429e-8a45-93e92ea505f4', distributed).
narrative_ontology:cs_authority_grounding('88131356-acba-429e-8a45-93e92ea505f4', lineage).
narrative_ontology:cs_interpretation_layer_present('88131356-acba-429e-8a45-93e92ea505f4').
narrative_ontology:cs_reading_relation('88131356-acba-429e-8a45-93e92ea505f4', remonstrance_authority__crown_reading, forecloses).
narrative_ontology:cs_axiom('88131356-acba-429e-8a45-93e92ea505f4', foundational, remonstrance_right_is_fundamental_law).
narrative_ontology:cs_axiom_status(remonstrance_right_is_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('88131356-acba-429e-8a45-93e92ea505f4', remonstrance_right_is_fundamental_law, deontological).
narrative_ontology:cs_axiom('88131356-acba-429e-8a45-93e92ea505f4', secondary, magistracy_constitutional_guardianship).
narrative_ontology:cs_axiom_status(magistracy_constitutional_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('88131356-acba-429e-8a45-93e92ea505f4', magistracy_constitutional_guardianship, conventional).
narrative_ontology:cs_reference_frame('88131356-acba-429e-8a45-93e92ea505f4', ancient_constitution_fundamental_laws).
narrative_ontology:cs_drift_state('88131356-acba-429e-8a45-93e92ea505f4', pre_revolutionary_fiscal_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('88131356-acba-429e-8a45-93e92ea505f4', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, tax_exempt_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, privileged_tax_orders).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, parlements_of_france).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, unprivileged_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_authority).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, fundamental_laws_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, ancient_constitution_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign courts — above all the Parlement of Paris — run the registration ritual: a royal edict has no force in their jurisdiction until they enroll it, and they choose whether to remonstrate formally against an edict before enrolling it, delay it, or demand amendments. Their offices are venal property, bought and inherited, and their corporate self-understanding is guardianship of the kingdom's fundamental laws. When the crown overrides them — a lit de justice forcing enrollment, exile of magistrates to distant towns, or wholesale suppression of the courts as in 1771 — the corps bears those costs directly and its members' office property loses value.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlements_of_france, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlements_of_france, payer).

% The men who sit on the courts belong, through their offices, to a robe nobility exempt from the main direct taxes. The edicts the courts resist most stubbornly are precisely the fiscal reforms that would have extended taxation to the privileged; the class's exemption is protected by the veto its own members administer. A few individuals renounced exemption as a gesture of patriotism; for the corps as a whole, giving up the exemption would dissolve the boundary that defines the class. These are the same men as the courts, in their class capacity rather than their judicial capacity.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, tax_exempt_magistracy, beneficiary,
    institutional, generational, identity_locked, national).

% The royal government writes fiscal edicts and needs them enrolled to borrow against them and fund wars; blocked or delayed edicts leave debts unfunded and ministers disgraced. It can force enrollment and punish the courts, but each exercise of that power buys a legitimacy crisis and a martyr narrative, and the 1771 attempt to replace the courts had to be reversed at the next succession. It also collects a quieter benefit: as long as the courts visibly block reform, the crown can present itself as willing but thwarted — an alibi that spares it both reform and the convocation of the Estates-General, whose agenda it could not control.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscal_authority, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, crown_fiscal_authority, beneficiary).

% Commoners who pay the taille and the indirect taxes bear the direct-tax burden that the blocked reforms would have redistributed toward the privileged. They have no seat in the enrollment ritual, no remonstrance channel of their own, and no assembly — the Estates-General last met in 1614. Their realistic options are paying, evading at risk, or emigrating at ruinous cost.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, unprivileged_taxpayers, payer,
    powerless, biographical, trapped, national).

% The clergy and the sword nobility hold exemptions from the main direct taxes; the courts' remonstrances against fiscal reform defend these exemptions alongside the magistracy's own, and provincial estates and assemblies remonstrate in parallel. Rank and exemption are the same thing for these orders; volunteering the exemption up is thinkable as individual piety, not as corporate policy.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, privileged_tax_orders, beneficiary,
    organized, generational, identity_locked, national).

% Reform ministers, provincial reformers, and pamphleteers argue that consent to taxation belongs to a representative assembly of the kingdom rather than to self-appointed magistrates. They have no institutional channel: the Estates-General is not convoked, the provincial assemblies the crown experiments with are short-lived and crown-staffed, and the courts police the print sphere in which they write.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, representative_assembly_advocates, excluded,
    moderate, biographical, trapped, national).

% Later historians and political economists reconstruct the enrollment practice reign by reign, the doctrinal war between the delegation and fundamental-law theories, and the fiscal record of the blocked reforms. They sit outside every seat in the dispute and see the whole arc: the medieval verification origin, the eighteenth-century political transformation, and the class entanglement.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, tax_exempt_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In a monarchy without written constitutional review or a standing representative assembly, the registration-and-remonstrance ritual provided a structured channel through which royal legislation was contested, reasoned about, delayed, and sometimes amended before taking force. It coordinated the relationship between crown and corporate bodies over the force of law and gave the kingdom's customary order a procedural checkpoint against unilateral innovation.
% TRANSFER_FUNCTION: Moves legislative assent power from the crown to the sovereign courts (no edict takes force without enrollment); maintains the existing fiscal burden distribution by blocking reform edicts that would extend taxation to privileged classes, so unprivileged taxpayers continue bearing the disproportionate share; transfers the political-voice function to the non-elected magistracy, which claims to speak for a nation with no other organ; and, in override episodes, moves disciplinary costs — exile, office insecurity, court suppression — onto the magistrates.
% ABSENT_VOICES: The unprivileged taxpayers whose burden the veto freezes have no seat in the registration ritual and no remonstrance channel; the Estates-General, which would give them a collective voice, went unconvoked from 1614 to 1789. Reform ministers whose edicts were blocked (Turgot, Calonne) appear only as supplicants before the courts. Provincial populations subject to Parlement of Paris jurisdiction had no representation in it. They are absent because the arrangement's guardianship claim substitutes for their voice — the magistracy speaks for the nation precisely where the nation cannot speak.
% DISAPPEARANCE_RATIONALE: The arrangement's actual disappearance rearranged everything: when the Estates-General met in 1789 and the National Assembly abolished privilege (August 4) and the courts' political function (November 1789), taxation became consent-based through representation, the venal office market collapsed, the tax-exempt magistracy's class position dissolved, and the crown's fiscal constitution was rewritten. Nothing about the fiscal-political order survived the arrangement's removal in its prior form.
% FOUNDING_PROBLEM: The practice descends from medieval registration custom: royal ordinances required verification by the sovereign courts for conformity with local customary law before taking force in a region — a legal-technological need of a fragmented customary order in which royal law was not self-executing.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians of royal legislation and registration practice — writing entirely outside the beneficiary set — attest that the verification function was obsolete by the eighteenth century: royal law's supremacy and the unification of the legal order removed the need for custom-verification, and the arrangement persisted performing a transformed political function. The magistracy itself attested a different founding problem (guardianship of ancient liberties); the crown, the reform ministries, and the pamphlet literature explicitly denied that attestation. No source outside the beneficiary class corroborates the transformed problem in the magistracy's form.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, ExtMetricName, E),
    domain_priors:suppression_score(remonstrance_authority__magistrate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(remonstrance_authority__magistrate_reading),
    narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end, matching the T38 series point) because the arrangement's operation on its fiscal face preserved exemption rents for the robe magistracy and the privileged orders and imposed real override costs on the crown — the manifest delta's high-epsilon-for-fiscal-reform-edicts. Suppression is high (0.78) and structural rather than internalized: the arrangement foreclosed the representative channel (the magistracy's guardianship claim substituted for a convocation the crown had no other reason to avoid), and the courts policed the print sphere in which reform arguments circulated. Theater rises from 0.22 to 0.55: the registration function was real throughout, but the guardianship rhetoric became increasingly performative relative to it as the fiscal conflicts intensified. Accessibility_collapse 0.55 and resistance 0.72 are descriptive of a contested institutional arrangement: the crown demonstrably could override and restructure (lit de justice, the exiles, Maupeou's 1771 abolition of the old courts), so alternatives did not collapse completely, but every exercise of those alternatives met massive organized resistance. The measurement series run on one shared grid of ten points (T0=1750, T5=1755, T10=1760, T15=1765, T20=1770, T22=1772, T25=1775, T30=1780, T35=1785, T38=1788) so every metric is authored at every examined time point. The series are deliberately cyclical rather than monotonic: the interval contains roughly two full cycles of fiscal crisis, confrontation, override or suppression, reconciliation, and renewed accumulation — the war-finance cycle of the 1750s-60s, the Maupeou cycle of 1770-74, and the terminal crisis of 1787-88. The oscillation is itself partly an extraction mechanism: each reconciliation (the 1774 recall above all) re-entrenched the privilege structure the veto protects, an intermittent-reinforcement dynamic in which the crown's capitulations raised the veto's price. The T22 dip in extractiveness records the Maupeou interlude — the old arrangement's operation was suspended while the replacement courts enrolled without remonstrance (the extraction of that interlude, the forced sale of new offices, belongs to a different constraint) — and the T22 spike in suppression_requirement records that holding the old arrangement in place briefly required more force than any holder would pay, which is why it was scrapped rather than maintained. Coalition note: the powerless payer seat's coalition potential materialized exactly once — when the crown, capitulating in 1788, convoked the Estates-General it had spent decades avoiding; the Third Estate's coalition then dissolved the arrangement from outside its own channels.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the magistracy's seat the arrangement is the constitution itself: the corps' identity is fused with guardianship — professional identity through patrimonial office, institutional identity in which the court has become the fundamental-law function, ideological identity in the ancient-constitution worldview — and its exit is identity_locked: a magistrate who enrolls without remonstrating dissolves the value of his office and the self-conception of his corps. From that seat the arrangement cannot register as extractive without self-dissolution. From the crown's seat the same arrangement is a negotiable obstacle that pays an alibi dividend: obstruction by the courts excuses non-reform and non-convocation, so the crown is simultaneously payer and beneficiary. From the taxpayers' seat the arrangement is invisible — a ceiling on reforms they never see proposed, borne as a burden that simply persists. If the identity frame broke — as it did when the Estates-General finally gave the nation an organ the magistracy did not control — the guardianship claim would dissolve and the privilege-protection function would stand exposed; that is in fact the sequence of 1789.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (tax_exempt_magistracy, privileged_tax_orders) place those seats near the beneficiary end of d; victim declarations (parlements_of_france, unprivileged_taxpayers) place those seats near the target end. The crown is dual-positioned and derives mid-range: it pays override costs and blocked-reform costs but collects the alibi benefit. The parlements' d is phase-dependent — near-symmetric while administering an unmolested registration ritual, near-full-target during override episodes (exile, suppression) — and the victim declaration encodes the override phase per the manifest delta; the engine's single per-seat d therefore reads the arrangement from the seat's worst structural phase, which is the honest reading of what the arrangement does to the corps that administers it. No directionality overrides are authored: the derivation from declarations and exits captures the structure, and a power-atom-keyed override could not separate the crown's dual position from the magistracy's beneficiary position, since both sit at the institutional power atom. The excluded advocates sit outside the derivation: their exclusion is the enforcement object, not a declared cost or benefit. Identity-locked exits on the magistracy and privileged orders push both toward the trapped end of the derivation despite their institutional power — power without exit does not lower effective extraction for a seat that cannot leave.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's founding problem — verifying royal edicts against local custom in a fragmented legal order — was dead by the eighteenth century; the arrangement persisted performing a transformed function (political veto plus privilege protection). The classification prevents mislabeling in both directions: reading the arrangement as pure extraction (the crown reading's verdict) erases the genuine check the courts performed against genuinely arbitrary fiscal innovation; reading it as the magistracy's own claim — an immemorial fundamental mechanism, a mountain — erases the class extraction that rode on the check. The false-summit path holds both: the claimed mountain with a declared beneficiary class routes to the tangled coordination/extraction structure the manifest delta describes, and the R5 fields (dead founding problem, world-rearranging disappearance) carry the mandatrophy signal the legacy boolean used to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immemoriality_vs_construction,
    'Is the remonstrance right a genuine immemorial fundamental law of the kingdom, or a constructed institutional arrangement whose ''ancient'' character is doctrinal retrojection by the class that benefits from it?',
    'Archival reconstruction of registration practice reign by reign — the 1673 first-passage rule, its suspension under Louis XIV, the regent''s restoration in 1715, the episodic exiles — to determine whether continuous immemorial practice or episodic royal grant-and-revocation better fits the record.',
    'If constructed, the mountain claim fails and the false-summit reclassification toward a tangled coordination/extraction structure is confirmed; if the immemorial claim survives scrutiny, the magistrate reading''s fundamentality claim strengthens and part of the measured extraction re-reads as the price of a genuine constitutional check.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immemoriality_vs_construction, empirical, 'Whether the right''s ancient status is real or beneficiary retrojection (schema-required natural-law vs. constructed ambiguity for a mountain with declared beneficiaries).').

omega_variable(
    check_privilege_separability,
    'Is the constitutional-check function of the remonstrance structurally separable from the privilege-protection function, given that the same act — blocking a fiscal edict — performs both?',
    'Compare the courts'' vigor across episode classes: fiscal-reform cases versus jurisdictional, disciplinary, and criminal-appeal cases where no fiscal privilege was at stake. If resistance intensity tracks fiscal stakes rather than arbitrariness of the edict, the functions are inseparable in practice.',
    'If separable, part of the measured extraction is the price of a real check and the tangled reading holds with a genuine coordination core; if inseparable, the arrangement is privilege-protection with constitutional cover and the extraction estimate rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(check_privilege_separability, conceptual, 'Whether the check function and the class-protection function can be pried apart.').

omega_variable(
    magistrate_reading_committer_structure,
    'This constraint is one reading (magistrate_reading) of the remonstrance_authority kernel; what would the sibling crown_reading change structurally, and where exactly is the disagreement located?',
    'The disagreement is located in the right''s SOURCE: fundamental law prior to and independent of royal will (this reading) versus revocable royal delegation (crown_reading). Adopting the crown reading would restructure this constraint''s victim set — overridden Parlements become a lawfully revoked concession rather than a victimized check — and would re-author epsilon over the same referent as near-pure usurpation, moving the classification from the false-summit region toward a pure-extraction profile.',
    'Cross-reading comparison is valid only over the shared referent (the standing registration-and-remonstrance arrangement); classification differences between the sibling files measure the readings, not the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magistrate_reading_committer_structure, conceptual, 'Committer structure: kernel identity, this reading, sibling structural delta, and the locus of disagreement.').

omega_variable(
    representation_substitution_ambiguity,
    'Did the remonstrance arrangement actively suppress the representative alternative (convocation of the Estates-General), or did the crown''s own aversion to convoking it explain its absence independently of the courts'' veto?',
    'Crown deliberative records and ministerial correspondence: did the crown skip the Estates because the Parlements made it unnecessary, or for independent reasons? Magisterial statements on whether the courts'' guardianship substitutes for representation.',
    'If the magistracy strategically suppressed representation, the arrangement''s suppression is higher than the structural measure suggests and the voice-of-the-nation claim is exposed as exclusion; if the crown alone avoided the Estates, the arrangement''s suppression drops and the crown bears more of the foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(representation_substitution_ambiguity, empirical, 'Whether the courts'' guardianship displaced or merely pre-empted representation.').

omega_variable(
    fiscal_reform_evaluation,
    'Whether blocking fiscal-reform edicts to preserve exemption counts as constitutional checking (protecting subjects from arbitrary taxation) or as class entrenchment (freezing an inequitable burden distribution) depends on evaluative weights between procedural anti-arbitrariness and distributive equity that no structural measurement settles.',
    'Explicit normative weighting per seat: how much weight a seat gives to checks against crown innovation versus equity in taxation; the magistrate and crown readings weight these differently, which is part of the kernel contest itself.',
    'The same descriptive record supports a coordination-heavy or extraction-heavy verdict depending on weights; the preference disagreement is signal about the readings, not measurement error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_reform_evaluation, preference, 'Evaluative under-determination of the check-versus-extraction verdict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remonstrance_magistrate_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t0, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t5, remonstrance_authority__magistrate_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t5, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t10, remonstrance_authority__magistrate_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t10, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t15, remonstrance_authority__magistrate_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t15, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t20, remonstrance_authority__magistrate_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t20, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t22, remonstrance_authority__magistrate_reading, theater_ratio, 22, 0.58).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t22, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t25, remonstrance_authority__magistrate_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t25, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t30, remonstrance_authority__magistrate_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t30, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t35, remonstrance_authority__magistrate_reading, theater_ratio, 35, 0.48).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t35, observed).
narrative_ontology:measurement(remonstrance_magistrate_tr_t38, remonstrance_authority__magistrate_reading, theater_ratio, 38, 0.55).
narrative_ontology:measurement_basis(remonstrance_magistrate_tr_t38, observed).

% Extraction over time
narrative_ontology:measurement(remonstrance_magistrate_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t0, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t5, remonstrance_authority__magistrate_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t5, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t10, remonstrance_authority__magistrate_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t10, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t15, remonstrance_authority__magistrate_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t15, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t20, remonstrance_authority__magistrate_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t20, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t22, remonstrance_authority__magistrate_reading, base_extractiveness, 22, 0.28).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t22, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t25, remonstrance_authority__magistrate_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t25, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t30, remonstrance_authority__magistrate_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t30, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t35, remonstrance_authority__magistrate_reading, base_extractiveness, 35, 0.72).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t35, observed).
narrative_ontology:measurement(remonstrance_magistrate_be_t38, remonstrance_authority__magistrate_reading, base_extractiveness, 38, 0.78).
narrative_ontology:measurement_basis(remonstrance_magistrate_be_t38, observed).

% Suppression requirement over time
narrative_ontology:measurement(remonstrance_magistrate_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t0, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t5, remonstrance_authority__magistrate_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t5, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t10, remonstrance_authority__magistrate_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t10, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t15, remonstrance_authority__magistrate_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t15, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t20, remonstrance_authority__magistrate_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t20, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t22, remonstrance_authority__magistrate_reading, suppression_requirement, 22, 0.9).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t22, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t25, remonstrance_authority__magistrate_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t25, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t30, remonstrance_authority__magistrate_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t30, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t35, remonstrance_authority__magistrate_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t35, observed).
narrative_ontology:measurement(remonstrance_magistrate_su_t38, remonstrance_authority__magistrate_reading, suppression_requirement, 38, 0.78).
narrative_ontology:measurement_basis(remonstrance_magistrate_su_t38, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, crown_reading).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, venal_office_property_system).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel decomposes into two constraint files because the colloquial label 'the remonstrance right' conflates two structurally distinct claims over one referent: this file (magistrate_reading) authors the right as fundamental constitutional mechanism, with the tax-exempt magistracy as beneficiary class and the overridden Parlements in the victim set; crown_reading authors the same arrangement as illegitimate minoritarian veto, with epsilon assessed as near-pure usurpation and the override episodes re-read as lawful revocation rather than victimization. The readings disagree on the right's source (fundamental law versus royal delegation), so no single file can hold both epsilon values — per the epsilon-invariance principle they are separate stories linked here. This arrangement also structurally influences the venal-office property system: Parlement powers underwrote office prices, so the enforcement history registered in this file's measurement series moved office values; the Maupeou episode is the sharpest case and is a candidate neighbor story in its own right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

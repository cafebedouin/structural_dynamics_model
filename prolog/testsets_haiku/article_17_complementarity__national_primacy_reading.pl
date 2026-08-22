% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity (National Primacy Reading)
 *   domain: international_law/criminal_justice
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes the ICC's complementarity
 *   principle: the Court shall declare a case inadmissible if a state is
 *   willing and able to genuinely investigate and prosecute. The national
 *   primacy reading interprets this to mean national courts are presumptively
 *   adequate, and the ICC bears the burden of proving unwillingness or
 *   inability — a high bar requiring demonstration of complete judicial
 *   collapse or deliberate impunity, not merely structural weakness or
 *   systemic bias. This reading protects national sovereignty and preserves
 *   state control over prosecution of international crimes, but restricts
 *   victim access to international remedies to cases of total state failure.
 *   The measurement series track how this reading has been operationalized
 *   over 24 years: extractiveness rising as the ICC applies the presumption
 *   more stringently, theater rising as the procedural performance of the
 *   adequacy-screening itself becomes a larger share of enforcement activity
 *   (fewer cases proceed to merits; more resource spent on admissibility
 *   contests), and suppression rising as the burden on prosecutors to
 *   overcome the presumption intensifies.
 *
 * KEY AGENTS:
 *   - National judiciaries (beneficiary + agenda-setter; institutional power; set the standard for adequacy; retain primacy unless proven sham)
 *   - Sovereignty-maximizing states (beneficiary; powerful; gain immunity from ICC intervention unless system collapses; ICC burden to intervene shifts to them)
 *   - Victims in weak-but-functioning systems (payer; powerless; trapped in domestic remedies; cannot reach ICC without proving exhaustion + state unwillingness)
 *   - Victims of political prosecution (payer; powerless; face domestic prosecution dressed in international-crime language; high inadmissibility threshold locks them in)
 *   - ICC Prosecutor (payer; institutional; must prove unwillingness or inability; constrained by presumption)
 *   - International accountability advocates (excluded; organized; argue for broader ICC reach; excluded from admissibility determination)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.68).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.71).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity (National Primacy Reading)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '6b766ad1-ad58-4b91-a06f-b11ee085e26e').
narrative_ontology:cs_kernel_codification('6b766ad1-ad58-4b91-a06f-b11ee085e26e', fixed_text).
narrative_ontology:cs_authority_grounding('6b766ad1-ad58-4b91-a06f-b11ee085e26e', lineage).
narrative_ontology:cs_interpretation_layer_present('6b766ad1-ad58-4b91-a06f-b11ee085e26e').
narrative_ontology:cs_reading_relation('6b766ad1-ad58-4b91-a06f-b11ee085e26e', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('6b766ad1-ad58-4b91-a06f-b11ee085e26e', foundational, national_primacy_sovereignty_foundational).
narrative_ontology:cs_axiom_status(national_primacy_sovereignty_foundational, holdable).
narrative_ontology:cs_axiom_grounding('6b766ad1-ad58-4b91-a06f-b11ee085e26e', national_primacy_sovereignty_foundational, deontological).
narrative_ontology:cs_axiom('6b766ad1-ad58-4b91-a06f-b11ee085e26e', secondary, presumption_of_adequacy_operational).
narrative_ontology:cs_axiom_status(presumption_of_adequacy_operational, holdable).
narrative_ontology:cs_axiom_grounding('6b766ad1-ad58-4b91-a06f-b11ee085e26e', presumption_of_adequacy_operational, conventional).
narrative_ontology:cs_reference_frame('6b766ad1-ad58-4b91-a06f-b11ee085e26e', national_court_presumptive_legitimacy).
narrative_ontology:cs_drift_state('6b766ad1-ad58-4b91-a06f-b11ee085e26e', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6b766ad1-ad58-4b91-a06f-b11ee085e26e', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_but_functioning_systems).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_political_prosecution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_prosecutor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority to prosecute international crimes occurring within their territory or involving their nationals. The complementarity principle as interpreted through national primacy grants them a presumption of adequacy — they control what 'willing and able' means in practice, and the ICC must affirmatively prove them unwilling or unable before intervening. They set evidentiary standards for admissibility, structure proceedings, and define scope of investigation without external override unless they completely fail.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter).

% Preserve sovereign immunity and freedom from external criminal adjudication of state officials and nationals. The national primacy reading protects them from ICC intervention unless their judicial systems collapse entirely — a high bar that preserves state control over prosecution of their own actors even when those actors face serious allegations. Non-ICC states in this category benefit most; ICC states with weak judiciaries retain protection through the presumption.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    powerful, generational, arbitrage, global).

% Seek accountability in a jurisdiction whose courts are underfunded, biased, or inefficient but not completely defunct. The national primacy reading treats such systems as presumptively adequate, leaving victims dependent on flawed domestic remedies they cannot access effectively. They cannot reach the ICC unless they first exhaust (often futile) domestic proceedings and then prove the state unwilling or unable — a threshold that requires showing deliberate impunity or total collapse, not mere inadequacy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_but_functioning_systems, payer,
    powerless, biographical, trapped, national).

% Face politically-motivated prosecution by a national system using international-crime law as a veneer for domestic political persecution. The high inadmissibility threshold locks them into domestic remedies even when the state is demonstrably unwilling (but maintains the theater of proceedings) — the reading's demand for proof of unwillingness rather than structural conflict of interest shifts the burden against them.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_political_prosecution, payer,
    powerless, biographical, trapped, national).

% Must affirmatively prove a state unwilling or unable before acting on a complaint — the reading places the burden of certification on the ICC, not on the state to demonstrate willingness. They accumulate evidentiary requirements, face resistance from states claiming adequacy, and cannot act on systemic failures that don't rise to complete judicial collapse. Their investigation resources are constrained by the presumption they must overcome.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_prosecutor, payer,
    institutional, generational, constrained, global).

% Argue that complementarity should trigger ICC intervention when national systems are demonstrably inadequate or biased, not merely when they completely fail. They are excluded from the decision to enforce the reading: national judiciaries and powerful states set the threshold; ICC actors administer it; victim-groups have no structural voice in whether the presumption applies to their situation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_accountability_advocates, excluded,
    organized, biographical, constrained, global).

% Examines how the reading operationalizes the text and where its structural consequences diverge from the coordinating function it claims to serve.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a two-tier criminal accountability system: national courts have primary responsibility; the ICC acts only as a backstop when national systems completely fail. This avoids simultaneous overlapping prosecutions, respects state sovereignty, and preserves incentives for national capacity-building by making international intervention a reputational and jurisdictional penalty for state failure.
% TRANSFER_FUNCTION: Transfers investigative and prosecutorial authority from a victim group (those seeking accountability) to national judiciaries and the state apparatus. The transfer occurs through a presumption: unless the state is demonstrably unwilling or unable, the ICC must defer and victims must work within domestic systems. National governments receive deference; the ICC receives a certification burden; victims receive subordination.
% ABSENT_VOICES: Victims in jurisdictions with weak but functional systems have no structural voice — their ability to reach the ICC depends on first exhausting domestic remedies and then meeting the high bar of proving state unwillingness, and they cannot participate in the admissibility determination. Civil society organizations advocating for broader ICC reach are also excluded: the decision to apply the national primacy reading rests with the ICC itself (constrained by the reading's presumption) and national states (beneficiaries of the presumption).
% DISAPPEARANCE_RATIONALE: If this reading of complementarity disappeared — if the ICC applied a lower threshold for triggering its own jurisdiction when national systems were demonstrably inadequate but not collapsed — victim access to international remedies would expand significantly, state sovereignty over prosecution would narrow, and the ICC's workload and political pressure would increase. National governments would lose the presumption of adequacy; victims in weak systems would gain direct access without exhaustion requirements.
% FOUNDING_PROBLEM: The Rome Statute framers sought to avoid simultaneous overlapping prosecutions of the same actors by different jurisdictions (duplicative trials, conflicting verdicts, prosecutorial races). They also sought to avoid imperialism: a global criminal court should not substitute for national capacity but rather incentivize and support it. Complementarity as national primacy embodies this as a structural rule: respect state sovereignty, presume national adequacy, and intervene only when states have completely abdicated.
% FOUNDING_PROBLEM_CORROBORATION: State delegations to the ICC and commentators defending national primacy attest the founding problem is live and the reading protects against both duplicative trials and neo-colonial interventionism. International human rights organizations and victim advocacy groups attest the founding problem has been solved by procedural safeguards (one-case-per-actor rules, complementarity screening itself) and that the reading now serves only to shield inadequate national systems from accountability. The divergence is structural, not empirical — the parties disagree on whether state sovereignty or victim access should weight more heavily.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the reading systematically transfers authority from victim-groups and the ICC to national states and judiciaries, and that transfer is enforced through a presumption that requires affirmative proof to overcome. The presumption operates as a structural barrier: victims must exhaust domestic remedies first, then prove state unwillingness — a sequencing that advantages the state. Suppression is high (0.71) because the reading actively restricts ICC intervention even when national systems are demonstrably inadequate; the suppression is structural (the presumption itself) and operationalized (prosecutors face documented resistance from states claiming adequacy). Theater is moderate-high (0.42) because admissibility determinations consume increasing shares of ICC resources — the procedural machinery of proving unwillingness/inability now dominates the investment, overshadowing the coordination function (preventing duplication, incentivizing national capacity) that originally justified complementarity. The measurement series show all three metrics rising steadily: as the reading has been applied, its extractive character has hardened, theatrical elements have grown (procedural contests replacing substantive adjudication), and suppression of ICC action has intensified. Accessibility collapse is moderate (0.62): alternatives to the national system theoretically exist (alternative forums, advocacy, universal jurisdiction in third countries) but are practically unavailable to most victims due to resource constraints and the ICC's monopoly on legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the national judiciary and state perspective, the constraint is protective of sovereignty and prevents imperialism. From the victim perspective in a weak system, it is extractive — authority is transferred to an often-hostile or incompetent apparatus they cannot exit. From the ICC prosecutor perspective, it is suppressive and resource-consuming — they must prove negatives (state unwillingness) rather than act on positives (state inadequacy). The engine should compute three different types across these seats from the structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are the structural beneficiaries (d near 0.0 — they gain authority, lose external review, receive the presumption). They have institutional power and arbitrage-level exit (they can withdraw from the ICC; the threat of withdrawal is what enforcement-against-the-presumption would cost them). Victims in weak systems and victims of political prosecution are the structural payees (d near 1.0 — they lose authority over their own cases, are locked into domestic systems they cannot exit, and must meet an onerous burden to override the presumption). They have powerless status and trapped exit. The ICC prosecutor sits at d ≈ 0.6–0.7 (bears the burden but retains some strategic discretion; organized institutional power but constrained by the presumption). No override is needed; the derivation from beneficiary/victim + exit should produce accurate d values across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is preventing duplicative prosecutions and respecting state sovereignty. The reading solves the duplication problem through structural deference (national courts presumptively adequate). But the sovereignty part has drifted: the reading now operates primarily to protect state actors from accountability, not to respect state capacity. A state that maintains theater prosecutions (nominal trials, light sentences, procedural delays) can invoke the presumption indefinitely because the text says 'willing and able,' and willingness is performatively maintained. The theater_ratio rising from 0.25 to 0.42 shows this drift: the constraint's operation is increasingly divorced from its coordinating function and increasingly tied to theater-preservation. A state that runs genuinely independent courts but reaches verdicts the ICC dislikes still benefits from the presumption because the reading prioritizes form (courts exist, proceedings occur) over function (accountability outcomes, deterrence effects). Mandatrophy here is the decoupling of the constraint's title (complementarity to support national capacity) from its effect (complementarity as state-sovereignty lock, regardless of national-capacity outcomes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    willingness_operationalization,
    'What distinguishes genuine unwillingness to prosecute from mere inadequacy of proceeding? Can a state maintain the formal appearance of willingness (initiating proceedings, staging trials) while running a sham system, and does the reading treat that as adequate?',
    'Empirical audit of case outcomes: compare states that meet the formal willingness criterion (proceedings initiated, trials held) versus outcome metrics (conviction rates adjusted for crime severity, sentence lengths, speed of verdicts). A high divergence between formal willingness and material accountability would show the reading permits systematic shams.',
    'If willingness is purely formal (proceedings occur regardless of outcome), the reading operates as a sovereignty lock, not an accountability principle. If willingness includes material accountability, the reading''s extractiveness would drop and resistance to it would lower. Current practice suggests willingness is formal, which hardens the reading''s extractive character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(willingness_operationalization, empirical, 'Whether willingness is measured by form (proceedings exist) or function (accountability outcomes).').

omega_variable(
    kernel_reading_contestation,
    'Is the national_primacy_reading a legitimate interpretation of Article 17, or does it foreclose the international_oversight_reading by requiring proof of state unwillingness rather than assessment of state adequacy?',
    'Textual and interpretive analysis: ''willing and able'' could mean (1) state demonstrates willingness through active prosecution (national_primacy reading), or (2) state demonstrates ability through functional adequacy of institutions (international_oversight reading). The two readings map to different baseline burdens: presumption-of-adequacy vs. presumption-of-inadequacy until proven otherwise.',
    'If the readings foreclose each other (one interpretation excludes the other logically), then the kernel contest is a binary choice: one reading wins, the other loses, and the sibling constraint''s classification reverses. If they coexist (different parties adopt different readings simultaneously), then both constraints are live, and the corpus needs both stories linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the two readings of complementarity foreclose each other or coexist as live interpretations.').

omega_variable(
    state_cooperation_extraction_mechanism,
    'Does the presumption of adequacy operate as a cooperation incentive (states perform better when given presumptive respect) or as an extraction mechanism (states perform worse because the ICC lacks leverage)?',
    'Comparative institutional analysis: states that are presumed adequate (and do not face ICC scrutiny) versus states that face immediate ICC jurisdiction (e.g., non-signatories where universal jurisdiction applies). If presumption-of-adequacy correlates with improved accountability outcomes, the reading supports its justification; if it correlates with reduced accountability (more impunity, weaker institutions), the reading is extractive despite its rationale.',
    'If extraction, the reading''s theater_ratio and suppression rise further; the reading is a sovereignty lock without the alleged coordination benefit. If cooperation-enabling, the reading''s extractiveness would be partially justified as a cost of incentivizing national capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_extraction_mechanism, empirical, 'Whether the presumption-of-adequacy incentivizes state performance or enables state impunity.').

omega_variable(
    victim_access_tradeoff,
    'Is victim access to the ICC supposed to be secondary to state cooperation (victims get remedies where states cooperate), or is victim access a primary entitlement that should be protected even if it requires ICC override of state preferences?',
    'Normative/preference analysis: the Rome Statute preamble emphasizes both state sovereignty and victim rights. The national_primacy_reading prioritizes sovereignty; an international_oversight_reading would prioritize victim access. No empirical fact resolves this — it is a question of which value should weight more heavily in the design of complementarity.',
    'If victim access is primary, the reading''s extractiveness is unjustified and the constraint should be reclassified or reframed. If state cooperation is primary, the reading''s treatment of victims as payees is rational (they bear the cost of protecting state sovereignty). This is the reading''s deepest normative commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_access_tradeoff, preference, 'Whether complementarity should prioritize state sovereignty or victim access to accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__national_primacy_reading, theater_ratio, 2002, 0.25).
narrative_ontology:measurement_basis(arti_tr_t2002, projected).
narrative_ontology:measurement(arti_tr_t2008, article_17_complementarity__national_primacy_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement_basis(arti_tr_t2008, observed).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__national_primacy_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement_basis(arti_tr_t2014, observed).
narrative_ontology:measurement(arti_tr_t2020, article_17_complementarity__national_primacy_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(arti_tr_t2020, observed).
narrative_ontology:measurement(arti_tr_t2026, article_17_complementarity__national_primacy_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__national_primacy_reading, base_extractiveness, 2002, 0.55).
narrative_ontology:measurement_basis(arti_be_t2002, projected).
narrative_ontology:measurement(arti_be_t2008, article_17_complementarity__national_primacy_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement_basis(arti_be_t2008, observed).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__national_primacy_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement_basis(arti_be_t2014, observed).
narrative_ontology:measurement(arti_be_t2020, article_17_complementarity__national_primacy_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement_basis(arti_be_t2020, observed).
narrative_ontology:measurement(arti_be_t2026, article_17_complementarity__national_primacy_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__national_primacy_reading, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement_basis(arti_su_t2002, projected).
narrative_ontology:measurement(arti_su_t2008, article_17_complementarity__national_primacy_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement_basis(arti_su_t2008, observed).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__national_primacy_reading, suppression_requirement, 2014, 0.67).
narrative_ontology:measurement_basis(arti_su_t2014, observed).
narrative_ontology:measurement(arti_su_t2020, article_17_complementarity__national_primacy_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(arti_su_t2020, observed).
narrative_ontology:measurement(arti_su_t2026, article_17_complementarity__national_primacy_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% Article 17 complementarity is a contested kernel with two structurally distinct readings. The national_primacy_reading treats complementarity as a sovereignty-protection mechanism with high inadmissibility threshold and state-presumed-adequate default. The international_oversight_reading treats complementarity as an accountability-trigger mechanism with lower inadmissibility threshold and ICC-presumed-necessary default. The readings coexist in live institutional dispute — different states, different coalitions within the ICC, and different advocacy communities endorse different readings. Each reading instantiates a distinct constraint with different beneficiaries, victims, and extractiveness profiles. They are linked by network.affects_constraints because the adoption or strength of one reading directly influences the operating conditions and legitimacy of the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, powerless, 0.95).
constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

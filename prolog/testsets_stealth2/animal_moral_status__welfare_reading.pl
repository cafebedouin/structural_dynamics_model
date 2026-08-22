% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Welfare Reading of Animal Moral Status: Sentience-Constrained Regulated Use
 *   domain: applied ethics/animal studies/legal philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   animal_moral_status: the welfare reading, under which animals are
 *   sentient beings whose suffering should be minimized within systems of
 *   regulated use, cruelty is wrong, and use is permissible. The constraint
 *   is the standing arrangement that reading produces and sustains: statutory
 *   anti-cruelty law plus welfare regulation (licensing, inspection,
 *   humane-slaughter rules, codes of practice) layered over a continuing
 *   regime of animal use. Per the kernel-reading epsilon rule, the referent
 *   of extractiveness is the standing regulated-use arrangement under
 *   contest, assessed by the welfare reading's own lights — how far actual
 *   practice falls short of the reading's own minimization promise — never
 *   the abolitionist arrangement the reading rejects nor the property
 *   arrangement it amends. The sibling readings (property_reading,
 *   abolitionist_reading) are separate constraints in separate files with
 *   their own victim sets and epsilon values; they are linked here only
 *   through network and omega structures. KEY AGENTS (by structural
 *   relationship): farmed_animals and laboratory_animals are the targets
 *   (powerless/trapped) bearing lawful suffering; regulated_use_industries is
 *   the primary beneficiary (institutional/constrained) collecting social
 *   license while paying compliance costs; animal_welfare_organizations is
 *   beneficiary with agenda-setting reach (organized/mobile);
 *   government_welfare_regulators is the agenda setter
 *   (institutional/constrained); consumers_of_animal_products sits
 *   near-symmetric (organized/constrained); abolitionist_advocates is the
 *   excluded voice (organized/mobile); welfare_science_bodies is the
 *   analytical observer (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.48).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.55).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Welfare Reading of Animal Moral Status: Sentience-Constrained Regulated Use").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied ethics/animal studies/legal philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '8c600bc2-2b88-46d2-ad54-01a7ee53c2cc').
narrative_ontology:cs_kernel_codification('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', formalized).
narrative_ontology:cs_authority_grounding('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', expertise).
narrative_ontology:cs_interpretation_layer_present('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc').
narrative_ontology:cs_reading_relation('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', foundational, sentience_grounds_moral_consideration).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', sentience_grounds_moral_consideration, deontological).
narrative_ontology:cs_axiom('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', foundational, humane_use_is_permissible).
narrative_ontology:cs_axiom_status(humane_use_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', humane_use_is_permissible, instrumental).
narrative_ontology:cs_reference_frame('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', regulated_use_sentience_compromise).
narrative_ontology:cs_drift_state('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', contemporary_welfare_washing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c600bc2-2b88-46d2-ad54-01a7ee53c2cc', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_use_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, laboratory_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, regulated_use_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, incremental_welfarism_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, sentience_property_law_compatibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raised, transported, and slaughtered under legally sanctioned humane-use standards. Their suffering is bounded by welfare rules such as stunning requirements, space allowances, and transport time limits, but remains substantial and lawful within those bounds. They cannot consent to or refuse their conditions, and nothing they do alters the standards applied to them.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Used in scientific research under harm-benefit analysis and the 3Rs (replacement, reduction, refinement). Procedures causing pain are licensed when researchers justify their necessity; the animals undergo the procedures regardless of the quality of the justification.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Farming, meat processing, pharmaceutical research, and entertainment businesses whose operations depend on using animals. Compliance with welfare standards costs money, but the standards also supply the public assurance and legal certainty that keep their markets open. Industry bodies participate in drafting codes of practice and fund much of the applied welfare research. Leaving animal use would mean abandoning sunk capital and established product lines.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_use_industries, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulated_use_industries, payer).

% Campaign for stronger standards, bring private cruelty prosecutions, run public education programs, and hold seats on government advisory boards. Their income depends on demonstrating both that cruelty persists and that their programs reduce it. Many helped draft the standards they now monitor. They could redirect their missions to other causes, though brand identity is bound to animal welfare.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, animal_welfare_organizations, agenda_setter).

% Draft legislation, license procedures, inspect premises, and prosecute cruelty offenses under statutory mandates. Enforcement budgets are small relative to the number of regulated premises, so oversight leans heavily on industry self-reporting and third-party audit schemes. They answer politically to both industry constituencies and animal-protection voters.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, government_welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Buy animal products whose availability and price depend on the regulated-use system continuing. Welfare labels offer moral assurance at the point of purchase; most buyers cannot verify conditions behind the label and carry the moral residue of participation indirectly. Individually they can reduce consumption, but dietary habits are socially embedded and change is costly.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, consumers_of_animal_products, payer).

% Argue that use itself, however regulated, wrongs the animals used. They campaign outside the standard-setting process, which takes the permissibility of use as its fixed premise; their proposals enter public debate as radical alternatives rather than admissible amendments to the framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Produce the evidence base — welfare indicators, affective-state measures, stocking-density trials — that legislatures convert into standards. Their findings both legitimate the framework and periodically expose the distance between its promises and on-farm or in-lab practice.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_science_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets uniform minimum standards for the treatment of animals used in farming, research, transport, and slaughter. This solves a collective-action problem no consumer, producer, or charity can solve alone: it prevents competitors from gaining advantage by treating animals worse, and gives buyers assurance without requiring any change in their behavior.
% TRANSFER_FUNCTION: Moves the products of animal bodies — meat, dairy, eggs, experimental data, labor — from animals to industries and consumers under legality-preserving conditions. Moves donations, statutory advisory seats, and moral authority to welfare organizations. Moves reputational protection to regulated industries, which can represent themselves as meeting welfare standards. Moves the cost of the whole system — suffering and death within lawful parameters — to the animals, who hold no seat in the transfer.
% ABSENT_VOICES: The animals themselves: the framework adjudicates the interests of parties who cannot attend, appeal, or veto, and their interests enter only as translated by welfare-science proxies and human representatives. Abolitionist advocates are also absent from standard-setting, which treats the permissibility of use as a fixed premise rather than an open question.
% DISAPPEARANCE_RATIONALE: Overnight repeal would end cruelty prosecutions and minimum standards immediately. Industries would lose the public assurance that keeps their markets open and would scramble to rebuild trust through private certification. Welfare organizations would lose statutory footholds and prosecutorial roles. Retailers would suddenly face verification demands they currently outsource to the framework. Every seated party's arrangements depend on the framework's continued existence.
% FOUNDING_PROBLEM: Gratuitous cruelty — wanton beating, starvation, careless transport and slaughter of animals — visible enough by the early nineteenth century to produce the first anti-cruelty statutes, and later systematized when intensive farming made suffering industrial in scale; the 1965 Brambell Report is the modern frame's founding document.
% FOUNDING_PROBLEM_CORROBORATION: Industry bodies attest the founding problem is substantially solved: standards are met and audits passed. Peer-reviewed welfare science, undercover-investigation footage reported by news organizations, and government audit-office findings on enforcement gaps corroborate, from outside the beneficiary set, that lawful suffering remains widespread. Abolitionist scholarship attests that the deeper problem — use itself — was never this frame's target. No party outside the framework's beneficiaries attests that the problem is fully solved.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.48 at interval end) because the welfare reading's own lights register a real shortfall: lawful practice — intensive confinement, routine procedures, licensed research pain — causes more suffering than the minimization promise admits, yet the framework genuinely bounds the worst outcomes relative to an unregulated baseline. Suppression is moderate (0.55): criminal cruelty penalties and compulsory slaughter rules are real coercion, but enforcement is thin, leans on self-reporting, and exempts customary practices. Theater is just past half (0.53) and rising: welfare labels, third-party audits, and corporate commitments increasingly certify paperwork rather than experienced conditions (Goodhart drift). Accessibility collapse is low (0.35): understanding the framework does not close off alternatives — abolitionist politics, vegan practice, and stronger-law campaigns all remain live — which is characteristic of a construct that must be defended rather than a natural limit. Resistance is moderate (0.50): industry lobbying against expansion (including transparency-suppressing legislation), abolitionist rejection of the frame, and periodic public backlash. The temporal series run on ONE shared grid (T=0..60 in decade steps, T=0 anchored at the 1965 Brambell Report) with every tracked metric authored at every point. Base extractiveness traces a U-shape: high at T=0 when regulation was thin and suffering effectively unbounded, falling as welfare science and statute matured through the Five Freedoms era, bottoming near T=40, then creeping back up as enforcement plateaued while labeling proliferated. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: machinery built out steadily for four decades, then flattened and partially privatized into audit schemes — a rise-plateau profile, not a static picture. If theater_ratio crosses roughly 0.6 while enforcement continues privatizing, the framework's assurance function is drifting toward performance maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Seats inside the welfare frame compute differently. From the regulator and welfare-organization seats the arrangement is an achievement being extended: each tightening of a standard is progress, and the frame's existence is what makes protection possible at all. From the animal seats — reachable only through proxy testimony — the same arrangement is a ceiling that converts open-ended wrong into bounded, lawful, renewable suffering. From the industry seat it is a manageable bargain: compliance costs purchased social peace and market stability. The abolitionist seat, external to this story's frame, denies the bargain's terms entirely — but that verdict belongs to the sibling story, not this one. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Farmed and laboratory animals are declared victims with trapped exit and no power: they sit at the full-target end, and effective extraction is amplified for them. Regulated_use_industries are declared beneficiaries but carry a genuine secondary payer position (compliance costs); the derivation should place them well toward the beneficiary end because the social-license gain demonstrably exceeds compliance cost — their markets stay open because the framework exists. Animal_welfare_organizations are beneficiaries collecting legitimacy, funding, and statutory roles. Consumers are dual-positioned (beneficiary/payer): moral comfort and product access against prices and diffuse complicity — approximately symmetric. Government regulators are mildly beneficiary-side: mandate fulfillment and order against enforcement burden. No directionality_overrides are authored: the structural declarations capture these relationships, and the override surface keys on power atoms that several distinct agents share in this story (three organized-power agents with different directionalities), so a coarse override would misapply across seats. The dual-role nuance for industries and consumers is recorded here and in their stakeholder situations instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters unusually here because both neighboring mislabels are politically loaded. Reading the framework as pure rope adopts its self-image and hides the asymmetric extraction — lawful suffering borne by parties with no exit, benefits concentrated in industries and organizations. Reading it as pure snare adopts the abolitionist critique and erases measurable coordination gains — stunning mandates, transport limits, prosecuted cruelty — that reduced suffering relative to the unregulated baseline. Tangled rope holds both truths: genuine coordination function, active enforcement, identifiable beneficiaries AND victims. No mandatrophy resolution is declared: the founding problem (gratuitous cruelty, and beyond it the minimization promise) is contested rather than dead, so the mandate has not outlived its function. The drift risk runs the other way — toward piton — if enforcement keeps privatizing while labels proliferate: a framework maintained theatrically, administered by parties who could change it but bear less of its cost than the animals do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates only the welfare reading of the animal_moral_status kernel — do the property and abolitionist readings classify the same subject matter as structurally different constraints with different victim sets?',
    'Compile and compare the three sibling stories (animal_moral_status__property_reading, animal_moral_status__abolitionist_reading): divergence in victim-set membership, epsilon, and computed type localizes the disagreement structurally.',
    'Under the abolitionist reading, use itself enters the victim set and effective extraction rises sharply toward snare territory; under the property reading the victim set empties and the arrangement approaches a pure coordination device. This story''s low-to-moderate epsilon is reading-indexed, not topic-indexed — cross-reading comparison is valid only between files, never by averaging inside one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three rival readings of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    humane_threshold_authority,
    'Who sets the threshold at which suffering counts as ''minimized'' — the animals'' interests as measured by welfare science, or commercial viability as negotiated with industry?',
    'Trace standard-setting provenance: compare welfare-science recommendations at the time of adoption against the standards actually enacted, and quantify industry participation in code-drafting bodies.',
    'A commercially-set threshold converts the protective function into a licensing device and pushes the classification toward snare; a science-set threshold supports the tangled_rope reading and the current epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humane_threshold_authority, empirical, 'Whether the minimization threshold tracks animal interests or industry negotiability.').

omega_variable(
    customary_practice_constitivity,
    'Are exemptions for ''customary'' or ''normally-accepted'' agricultural practices an enforcement failure correctable within the framework, or constitutive of it — does the framework structurally defer to whatever industry already does?',
    'Compare prosecution and inspection records against documented lawful-but-severe practices; test whether reform proposals targeting customary practices repeatedly fail at the same procedural stage across jurisdictions.',
    'If constitutive, the framework''s protective function is thinner than claimed and effective extraction is understated by the current metrics; if correctable, the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_practice_constitivity, empirical, 'Whether the customary-practice exemption is a bug or the framework''s operating principle.').

omega_variable(
    welfare_label_verification_gap,
    'Do welfare labels and third-party audit schemes measure conditions animals actually experience, or compliance with documentation — and does the gap widen as schemes proliferate?',
    'Meta-analysis of audit-scheme outcomes against on-farm and in-lab welfare indicators collected independently of the schemes.',
    'A widening paperwork-versus-experience gap raises the functional significance of theater_ratio and dates a drift risk toward performance-maintained operation of the assurance function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_label_verification_gap, empirical, 'Whether the framework''s assurance instruments track animal experience or administrative compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__welfare_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__welfare_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__welfare_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(anim_tr_t60, animal_moral_status__welfare_reading, theater_ratio, 60, 0.53).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__welfare_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__welfare_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__welfare_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement(anim_be_t60, animal_moral_status__welfare_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__welfare_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__welfare_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__welfare_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(anim_su_t60, animal_moral_status__welfare_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'animal protection' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing the animal_moral_status kernel. The welfare reading (this file) layers sentience-constrained treatment rules OVER property status — it presupposes the property reading's legal substrate while contradicting the abolitionist reading's permissibility premise outright. Epsilon differs across the family because the victim set differs: empty for property, confined to cruel methods and lawful-but-severe practice for welfare, coextensive with all use for abolitionist. The upstream member is the property reading (the legal substrate all jurisdictions actually operate); the welfare reading modifies it; the abolitionist reading repudiates the modified whole. Each file links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

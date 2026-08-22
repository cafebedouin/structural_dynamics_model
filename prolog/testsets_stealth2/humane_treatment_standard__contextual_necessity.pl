% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual-Necessity Reading of the Humane Treatment Standard (Common Article 3)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets minimum
 *   humane-treatment guarantees for persons taking no active part in
 *   hostilities in non-international armed conflict. The contextual_necessity
 *   reading instantiates the arrangement in which that baseline is real but
 *   derogable: security agencies define when national security imperatives
 *   override it, designate categories of detainees (high-value targets,
 *   unlawful combatants) whose protections lapse, and operate interrogation
 *   programs behind that designation. This story authors THAT reading as a
 *   clean, epsilon-invariant constraint — the sibling readings
 *   (absolute_prohibition, proportionality_balancing) are separate files
 *   linked through the network, not folded into this one. The epsilon
 *   referent is the standing conditional-protection arrangement itself,
 *   assessed by this reading's own lights: the baseline is genuine and most
 *   detainees in most conflicts receive it, but the override channel
 *   concentrates severe costs on a category the benefiting agencies
 *   themselves draw. Claim/metric independence is preserved: claimed_type
 *   tangled_rope is my structural judgment (a real coordination floor plus
 *   asymmetric extraction, held together by active enforcement); the metrics
 *   are authored independently as descriptive fact.
 *
 * KEY AGENTS:
 *   - national_security_agencies: agenda-setter (institutional/arbitrage) — writes the interpretive opinions, draws the exclusion line, collects the intelligence product and the discretion itself
 *   - executive_branches: beneficiary (institutional/arbitrage) — authorizes programs, gains flexibility and deniability, bears episodic exposure
 *   - interrogation_program_operators: dual-positioned beneficiary/payer (organized/identity_locked) — execute the techniques, collect mission identity, absorb liability and injury
 *   - designated_high_value_detainees: primary target (powerless/trapped) — classified out of protection exactly when custody is harshest
 *   - conditionally_protected_detainees: secondary target with residual benefit (powerless/trapped) — hold a revocable floor
 *   - icrc_detention_monitors: observer with constrained voice (institutional/constrained) — confidential access traded against public objection
 *   - human_rights_litigators: excluded challenger (organized/mobile) — shut out of domestic forums, litigates abroad
 *   - treaty_supervisory_bodies: analytical observer (institutional/analytical) — interpretive authority without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.74).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.75).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.74).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual-Necessity Reading of the Humane Treatment Standard (Common Article 3)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'ac67035a-e990-47c5-a7ba-b19a73ca4b59').
narrative_ontology:cs_kernel_codification('ac67035a-e990-47c5-a7ba-b19a73ca4b59', fixed_text).
narrative_ontology:cs_authority_grounding('ac67035a-e990-47c5-a7ba-b19a73ca4b59', lineage).
narrative_ontology:cs_interpretation_layer_present('ac67035a-e990-47c5-a7ba-b19a73ca4b59').
narrative_ontology:cs_reading_relation('ac67035a-e990-47c5-a7ba-b19a73ca4b59', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('ac67035a-e990-47c5-a7ba-b19a73ca4b59', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('ac67035a-e990-47c5-a7ba-b19a73ca4b59', foundational, security_necessity_overrides_baseline).
narrative_ontology:cs_axiom_status(security_necessity_overrides_baseline, holdable).
narrative_ontology:cs_axiom_grounding('ac67035a-e990-47c5-a7ba-b19a73ca4b59', security_necessity_overrides_baseline, instrumental).
narrative_ontology:cs_axiom('ac67035a-e990-47c5-a7ba-b19a73ca4b59', secondary, humane_treatment_is_context_dependent).
narrative_ontology:cs_axiom_status(humane_treatment_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('ac67035a-e990-47c5-a7ba-b19a73ca4b59', humane_treatment_is_context_dependent, conventional).
narrative_ontology:cs_reference_frame('ac67035a-e990-47c5-a7ba-b19a73ca4b59', necessity_conditioned_baseline).
narrative_ontology:cs_drift_state('ac67035a-e990-47c5-a7ba-b19a73ca4b59', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ac67035a-e990-47c5-a7ba-b19a73ca4b59', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, executive_branches).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, designated_high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, conditionally_protected_detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, interrogation_program_operators).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, conditionally_protected_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, interrogation_program_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Common Article 3 for its own operations, issues the legal opinions that define when national security necessity permits departing from baseline treatment, decides which detainees fall outside protected categories, and runs the resulting interrogation programs. Collects the intelligence product, the doctrinal discretion, and the legal cover. When scrutiny rises it can relocate programs, rename techniques, or reframe doctrine — moving between legal frameworks faster than oversight can follow.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Authorizes programs through classified memoranda and appoints the lawyers who write the interpretive opinions. Gains policy flexibility and deniability while bearing episodic costs when programs surface: litigation, diplomatic friction, reputational damage. Its horizon is electoral cycles and historical legacy rather than the decades-scale life of the doctrine it set in motion.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, executive_branches, beneficiary,
    institutional, biographical, arbitrage, national).

% Carries out the authorized techniques under the legal cover the opinions provide. Receives career advancement, mission identity, and institutional belonging; absorbs personal legal exposure and lasting psychological injury when programs are later repudiated. Leaving the work means abandoning a professional identity built around the mission, and several who left faced ostracism or investigation rather than reintegration.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, interrogation_program_operators, beneficiary,
    organized, immediate, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, interrogation_program_operators, payer).

% Is classified out of baseline protection at the moment custody becomes harshest — held in undisclosed sites, subjected to the authorized techniques, denied counsel and forum for years. Testimony is often classified or discounted as coerced. There is no exit from the category except release, transfer, or death, and no petition route to the bodies that decide the classification.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, designated_high_value_detainees, payer,
    powerless, immediate, trapped, global).

% Holds nominal baseline protection on terms that can be revoked by reclassification. Receives the floor's guarantees — humane confinement, freedom from humiliating treatment — while knowing the same agencies that grant the floor decide who falls outside it. Lives in detention systems where the floor is honored unevenly depending on how salient the case is.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, conditionally_protected_detainees, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, conditionally_protected_detainees, beneficiary).

% Negotiates confidential access to detention facilities and documents treatment deviations in private reports to the detaining power. The access model trades public denunciation for continued entry, so the strongest findings circulate only among the parties monitored. Withdrawing from confidentiality would end access; remaining inside it mutes the objection.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, icrc_detention_monitors, observer,
    institutional, generational, constrained, global).

% Barred from detention sites and often from confirmed client contact. Litigates in foreign and international forums — regional human-rights courts, universal-jurisdiction complaints, United Nations mechanisms — because domestic forums close under state-secrets and standing doctrines. Filings shape the record even where they lose.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_litigators, excluded,
    organized, generational, mobile, continental).

% Reviews state reports, hears shadow submissions, and issues concluding observations on detention practice. Holds interpretive authority but no enforcement power; findings are contested, delayed, or dismissed by the states whose practice they assess. The seat is analytical: it sees the structure whole but cannot alter it directly.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, treaty_supervisory_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, national_security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared minimum-treatment floor for detainees in non-international armed conflict — the gap Common Article 3 was written to close — and, under this reading, a decision procedure for reconciling that floor with asserted security necessities.
% TRANSFER_FUNCTION: Moves bodily autonomy, procedural rights, and legal personhood from detainees reclassified as security threats to the security apparatus; moves interpretive discretion and legal cover to security agencies and executive branches; moves intelligence product upward to policymakers.
% ABSENT_VOICES: The detainees subjected to enhanced techniques have no seat in the interpretive process — their testimony is frequently classified, obtained under coercion, or excluded from court on state-secrets grounds. Independent medical and psychological professionals who examined former detainees were largely absent from the official reviews that shaped the reading. Legislatures in several states deferred to executive legal advisories without hearing detainee-side evidence.
% DISAPPEARANCE_RATIONALE: If the conditional-override structure vanished overnight, detention and interrogation practice would rearrange around whichever reading filled the vacuum: the absolute reading would criminalize existing programs and force release-or-prosecution decisions across the detainee population; the proportionality reading would transfer gatekeeping to courts. Program infrastructure, intelligence-sharing arrangements among allies, and the legal status of thousands of current detainees all depend on the present reading.
% FOUNDING_PROBLEM: Common Article 3 (1949) answered a real gap: the Geneva Conventions' prisoner-of-war regime did not reach non-international armed conflict, leaving detainees in civil wars and insurgencies with no shared minimum standard. The contextual-necessity reading was later built to solve a second problem asserted on top of the first: that absolute standards allegedly bind states facing transnational terrorist networks whose intelligence cannot be obtained through baseline-compliant interrogation.
% FOUNDING_PROBLEM_CORROBORATION: The 1949 protection gap is corroborated from outside any benefiting party: ICRC preparatory-work scholarship, the near-universal state accession to the Conventions, and the doctrinal consensus that preceded the necessity extension. The necessity-override extension is attested almost exclusively by the benefiting parties themselves — agency legal advisories and executive memoranda. ICRC commentaries, UN Committee Against Torture concluding observations, and regional-court judgments from outside the beneficiary set dispute both its legality and its necessity; retired interrogators and intelligence veterans split on whether the practices were ever operationally required.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because the costs concentrate on detainees whose protection lapses precisely when custody is most coercive, and because the eligibility line for that lapse is drawn by the party that collects the benefit — self-certified discretion is the deepest asymmetry in the structure. Suppression (0.75) is authored as a raw structural property and deliberately unscaled: captivity, classification regimes, closed forums, and the isolation-by-design that prevents detainee coalition formation; only extractiveness gets scaled by directionality and scope in the engine's computation, and the commentary keeps that distinction visible. Theater ratio (0.52) reflects a real floor wrapped in growing performance: compliance documentation, confidential-dialogue ritual, and definitional work ('enhanced,' 'not torture') that functions increasingly to defend the override rather than the floor. Accessibility collapse (0.60): the absolute-prohibition alternative remains live internationally, so alternatives are not fully collapsed at the doctrinal level — but within this reading's frame, detainee-side alternatives collapse completely. Resistance (0.58): sustained litigation, treaty-body findings, and regional-court judgments meet the arrangement without defeating it. The temporal series runs on ONE shared grid (1994, 1999, 2002, 2006, 2009, 2014, 2020, 2025) across all three tracked metrics. Suppression_requirement is included because the story specifically tracks enforcement-capacity change: machinery built up sharply 2002-2006 (secrecy architecture, site security, witness control), partially stood down 2009-2014 after program termination and forced disclosure, then re-hardened 2020-2025 (leak prosecutions, classification expansion). The pattern is an enforcement lifecycle, not an oscillating extraction cycle — no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same text. From the agencies' position the arrangement is a managed floor with lawful exceptions — coordination they built and staff. From the designated detainees' position the same structure operates as unconditional subjection: the floor vanishes exactly when it would matter, exit is nil, and the classification decision is made by their captors. Conditionally protected detainees sit between — holders of a revocable guarantee, which is experienced as protection whose continuance depends on staying unremarkable. Operators are genuinely dual-positioned: beneficiaries of the discretion and identity the program confers, payers of the exposure and injury it leaves behind. Treaty bodies and the ICRC see the structure whole but from seats with no enforcement purchase. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies and executive branches sit near the beneficiary pole: the override channel delivers intelligence product, doctrinal discretion, and legal cover to them, and their arbitrage-grade exit (relocating programs, renaming techniques, reframing doctrine) places them nearest the subsidized end. Designated high-value detainees sit at the full-target pole — trapped exit amplifies their effective extraction, and global spatial scope raises verification difficulty, which the engine scales upward. Conditionally protected detainees derive mid-to-high directionality: real floor benefits damp their extraction below the designated category's, but revocability keeps them firmly on the cost-bearing side. Operators mix both flows (benefit of mission identity, cost of exposure), landing near symmetric with identity lock binding them to the arrangement. Monitors and treaty bodies are analytical or constrained observers contributing little directional mass. Inter-institutionally, the same nominal power level divides sharply: agencies hold arbitrage exit while treaty bodies hold only analytical exit — identical institutional standing, opposite structural relationships to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure rope (the benefiting parties' framing: 'we maintained the baseline throughout') erases the categorical exclusion channel and the self-certified discretion that concentrate its costs. Reading it as pure snare erases the genuine floor that most detainees in most conflicts actually receive and that the 1949 drafting history shows was a real achievement. The R5 interview sharpens this: the founding problem (the non-international-conflict protection gap) is live and corroborated from outside the beneficiary set, but the necessity-override extension built on top of it is contested — its corroboration comes almost entirely from the parties that collect its benefits. Status=contested crossed with verdict=world_rearranges is exactly the configuration the mismatch consumer watches: the arrangement's persistence depends on parties disputing whether its added mandate still serves anything beyond the original floor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the humane_treatment_standard kernel — the contextual_necessity reading. How would the sibling readings restructure the constraint''s victim set, discretion allocation, and enforcement burden?',
    'Read against the sibling stories humane_treatment_standard__absolute_prohibition and humane_treatment_standard__proportionality_balancing: the diverging elements are whether the baseline is derogable at all, who decides when it yields, and whether exclusion from protection is categorical or case-determined.',
    'Under the absolute reading the lawful exclusion category is empty by design and the arrangement''s costs become outright violations; under the proportionality reading discretion migrates from security agencies to adjudicators and the victim set becomes case-determined rather than category-determined. This story''s classification holds only for the contextual reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    enhanced_interrogation_efficacy,
    'Is the empirical premise beneath the necessity override true — that enhanced interrogation produces actionable intelligence unobtainable through baseline-compliant methods?',
    'Declassified program assessments and cross-examination of the internal record: the Senate study concluded the practices were not effective and that comparable intelligence came from conventional methods; the operating agency disputed this. Independent replication of interrogation-outcome research would resolve it.',
    'If the efficacy premise fails, the instrumental grounding of the override collapses and the arrangement''s coordination story reduces to cover — pushing the computed type toward the snare end. If it holds, part of the measured cost is the price of the security function the reading exists to protect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enhanced_interrogation_efficacy, empirical, 'Whether the override''s instrumental justification survives its own evidentiary record.').

omega_variable(
    necessity_threshold_self_certification,
    'Is the necessity threshold an objective legal standard applied from outside, or a discretion the benefiting agencies effectively certify for themselves?',
    'Comparative analysis of threshold application across cases and jurisdictions: who determined that necessity existed, on what evidence, subject to what review, and with what reversal rate.',
    'If the threshold is self-certified, the arrangement''s asymmetry is deeper than the metrics suggest — the party collecting the benefit also draws the eligibility line for the victim category — supporting higher effective extraction. An externally adjudicated threshold would restore part of the structure to genuine rule-bound coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_threshold_self_certification, conceptual, 'Whether the override gate is a rule or a discretion.').

omega_variable(
    victim_set_expansion_ratchet,
    'Is the excluded-victim set (high-value targets, unlawful combatants) stable at its declared boundary, or does it ratchet outward under agency discretion?',
    'Longitudinal tracking of category membership criteria across the interval: the historical record already shows expansion from a narrow high-value category toward broader unlawful-combatant designations; continued monitoring of designation practice would confirm or bound the ratchet.',
    'Outward expansion converts the arrangement from a bounded exception into an open-ended channel — the tangled-rope reading degrades toward snare as the coordinated floor shrinks relative to the extracted population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_expansion_ratchet, empirical, 'Boundary stability of the excluded-victim category.').

omega_variable(
    fixing_cost_composition,
    'Is the cost of fixing this arrangement genuinely prohibitive, or is it political unwillingness by the seats with removal power, priced as cost?',
    'Counterfactual comparison with jurisdictions and moments where removal was attempted: executive orders terminating programs, judicial rulings rejecting the override, legislative restrictions — each shows the machinery CAN be changed at bounded cost when the will exists.',
    'If the cost is preference rather than structure, the prohibitive rating reflects captured incentives among fixers, and the arrangement is more tractable than its receipt-surface entry suggests — relevant to any remedy analysis downstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fixing_cost_composition, preference, 'Whether prohibitive fixing cost is structural or volitional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 1994, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1994, humane_treatment_standard__contextual_necessity, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(huma_tr_t1999, humane_treatment_standard__contextual_necessity, theater_ratio, 1999, 0.23).
narrative_ontology:measurement(huma_tr_t2002, humane_treatment_standard__contextual_necessity, theater_ratio, 2002, 0.31).
narrative_ontology:measurement(huma_tr_t2006, humane_treatment_standard__contextual_necessity, theater_ratio, 2006, 0.39).
narrative_ontology:measurement(huma_tr_t2009, humane_treatment_standard__contextual_necessity, theater_ratio, 2009, 0.45).
narrative_ontology:measurement(huma_tr_t2014, humane_treatment_standard__contextual_necessity, theater_ratio, 2014, 0.48).
narrative_ontology:measurement(huma_tr_t2020, humane_treatment_standard__contextual_necessity, theater_ratio, 2020, 0.51).
narrative_ontology:measurement(huma_tr_t2025, humane_treatment_standard__contextual_necessity, theater_ratio, 2025, 0.52).

% Extraction over time
narrative_ontology:measurement(huma_be_t1994, humane_treatment_standard__contextual_necessity, base_extractiveness, 1994, 0.45).
narrative_ontology:measurement(huma_be_t1999, humane_treatment_standard__contextual_necessity, base_extractiveness, 1999, 0.48).
narrative_ontology:measurement(huma_be_t2002, humane_treatment_standard__contextual_necessity, base_extractiveness, 2002, 0.62).
narrative_ontology:measurement(huma_be_t2006, humane_treatment_standard__contextual_necessity, base_extractiveness, 2006, 0.7).
narrative_ontology:measurement(huma_be_t2009, humane_treatment_standard__contextual_necessity, base_extractiveness, 2009, 0.66).
narrative_ontology:measurement(huma_be_t2014, humane_treatment_standard__contextual_necessity, base_extractiveness, 2014, 0.71).
narrative_ontology:measurement(huma_be_t2020, humane_treatment_standard__contextual_necessity, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(huma_be_t2025, humane_treatment_standard__contextual_necessity, base_extractiveness, 2025, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1994, humane_treatment_standard__contextual_necessity, suppression_requirement, 1994, 0.48).
narrative_ontology:measurement(huma_su_t1999, humane_treatment_standard__contextual_necessity, suppression_requirement, 1999, 0.52).
narrative_ontology:measurement(huma_su_t2002, humane_treatment_standard__contextual_necessity, suppression_requirement, 2002, 0.66).
narrative_ontology:measurement(huma_su_t2006, humane_treatment_standard__contextual_necessity, suppression_requirement, 2006, 0.78).
narrative_ontology:measurement(huma_su_t2009, humane_treatment_standard__contextual_necessity, suppression_requirement, 2009, 0.72).
narrative_ontology:measurement(huma_su_t2014, humane_treatment_standard__contextual_necessity, suppression_requirement, 2014, 0.69).
narrative_ontology:measurement(huma_su_t2020, humane_treatment_standard__contextual_necessity, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(huma_su_t2025, humane_treatment_standard__contextual_necessity, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the humane treatment standard of Common Article 3' covers three structurally distinct claims — non-derogable minimum (absolute_prohibition), conditioned baseline with necessity override (this story), and case-by-case balancing (proportionality_balancing). Each is authored as its own story with its own epsilon, beneficiary/victim structure, and classification; all three share the same referent (the operative conditional-protection arrangement) and author different epsilon by their own lights. This story links both siblings via affects_constraints; the absolute reading is upstream (it is the text's default doctrinal reading and the source the necessity reading must derogate from), and the proportionality reading is downstream of the factual terrain this reading created.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

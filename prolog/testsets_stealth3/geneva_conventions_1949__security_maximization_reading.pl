% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Security-Maximization Reading of the Geneva Conventions: the Necessity-Suspended-Protections Regime
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Geneva Conventions 1949
 *   kernel: the security-maximization reading, under which the Conventions
 *   are peacetime aspirations that yield to operational necessity in
 *   asymmetric conflict, licensing an expanded unlawful-combatant category,
 *   degraded civilian immunity via human-shields reasoning and collateral
 *   acceptance, indefinite detention without trial, and coercive
 *   interrogation reframed as non-torture. The sibling readings (humanitarian
 *   ceiling, conditional reciprocity) are separate constraints in separate
 *   files; nothing about them is averaged into this story. The ε referent is
 *   the standing arrangement under contest — the actual detention,
 *   interrogation, and targeting regime operated under this reading from 2001
 *   forward — described as I assess it descriptively. The claim and the
 *   metrics are independent: I claim tangled_rope because the arrangement
 *   carries a genuine doctrinal coordination function that militaries across
 *   history have sought, while the metrics describe heavily extractive,
 *   actively enforced operation whose gains concentrate in one seat.
 *
 * KEY AGENTS:
 *   - - executive_war_powers_authority: agenda setter (institutional/arbitrage) — writes the interpretations, collects the intelligence product and the accumulated discretion
 *   - - military_and_intelligence_operators: implementing beneficiary with payer residue (organized/identity_locked) — legal cover and rules in exchange for prosecution exposure, moral injury, reciprocity risk
 *   - - detained_suspected_combatants: primary payer (powerless/trapped) — status denied, custody indefinite, no counsel in the formative years
 *   - - civilian_populations_asymmetric_theaters: primary payer (powerless/trapped) — immunity degraded by proximity-and-shields reasoning
 *   - - captured_signatory_forces_reciprocity_exposed: diffuse payer (powerless/trapped) — their protection is the norm their own state is spending down
 *   - - jag_officers_and_internal_legal_dissenters: excluded insider voice (organized/identity_locked) — argued treaty violation and intelligence unreliability, overruled at career cost
 *   - - icrc_and_un_monitoring_bodies: excluded external monitor (institutional/constrained) — access denied where practices were sharpest
 *   - - domestic_and_supranational_judicial_review: observer (institutional/analytical) — episodic restoration of rights, no administration
 *   - - allied_governments_importing_precedent: secondary beneficiary (powerful/mobile) — imports the template, exits cheaply
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.79).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.68).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Security-Maximization Reading of the Geneva Conventions: the Necessity-Suspended-Protections Regime").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '2b776b0f-2650-4d6b-b584-a5e42d50444a').
narrative_ontology:cs_kernel_codification('2b776b0f-2650-4d6b-b584-a5e42d50444a', fixed_text).
narrative_ontology:cs_authority_grounding('2b776b0f-2650-4d6b-b584-a5e42d50444a', practice).
narrative_ontology:cs_interpretation_layer_present('2b776b0f-2650-4d6b-b584-a5e42d50444a').
narrative_ontology:cs_reading_relation('2b776b0f-2650-4d6b-b584-a5e42d50444a', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('2b776b0f-2650-4d6b-b584-a5e42d50444a', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('2b776b0f-2650-4d6b-b584-a5e42d50444a', foundational, military_necessity_overrides_treaty_protection).
narrative_ontology:cs_axiom_status(military_necessity_overrides_treaty_protection, holdable).
narrative_ontology:cs_axiom_grounding('2b776b0f-2650-4d6b-b584-a5e42d50444a', military_necessity_overrides_treaty_protection, instrumental).
narrative_ontology:cs_axiom('2b776b0f-2650-4d6b-b584-a5e42d50444a', foundational, combatant_status_requires_uniformed_state_affiliation).
narrative_ontology:cs_axiom_status(combatant_status_requires_uniformed_state_affiliation, holdable).
narrative_ontology:cs_axiom_grounding('2b776b0f-2650-4d6b-b584-a5e42d50444a', combatant_status_requires_uniformed_state_affiliation, conventional).
narrative_ontology:cs_reference_frame('2b776b0f-2650-4d6b-b584-a5e42d50444a', conventions_peacetime_aspirational_baseline).
narrative_ontology:cs_drift_state('2b776b0f-2650-4d6b-b584-a5e42d50444a', post_boumediene_contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2b776b0f-2650-4d6b-b584-a5e42d50444a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, executive_war_powers_authority).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_operators).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, allied_governments_importing_precedent).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_suspected_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_asymmetric_theaters).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, captured_signatory_forces_reciprocity_exposed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_operators).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, operational_necessity_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, unlawful_enemy_combatant_category).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, military_expertise_primacy_in_wartime_legal_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets detention, interrogation, and targeting policy for the armed conflict and commissions legal interpretations declaring which treaty protections apply where and to whom. Collects the intelligence product the programs generate and accumulates discretionary power each time a court or treaty body concedes ground. Has shown it can reverse components of the policy by unilateral directive, as when interrogation authorization changed by executive order in 2009, and can design around adverse rulings through reclassification, venue selection, and novel legal theories.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, executive_war_powers_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Run the detention facilities, conduct the interrogations, and execute the targeting plans under the doctrine. They receive clear rules of engagement, legal cover for actions that would otherwise expose them individually, and career advancement tied to intelligence production. They also carry prosecution exposure when policy shifts strand past conduct, moral injury reported by returning interrogators, and the knowledge that adversaries may mirror the treatment of their own captured comrades. Leaving the service mid-career forfeits pension and identity; dissent inside the institution has historically been absorbed or penalized rather than acted on.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_operators, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_operators, payer).

% Seized in raids or handed over by partner forces, classified outside prisoner-of-war categories, and held for years without charge or trial, often without counsel in the early years of custody. Subject to interrogation methods including stress positions, prolonged isolation, temperature manipulation, and simulated drowning. Cannot leave, cannot effectively appeal their classification, and cannot decline participation; their families frequently do not learn where they are held.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detained_suspected_combatants, payer,
    powerless, immediate, trapped, global).

% Live in districts where irregular fighters shelter among them. Bear strikes justified by reasoning that proximity to combatants or their presumed use as shields reduces the protection owed to them. Lose homes, livelihoods, and kin recorded as collateral harm. Have no forum in which their objection registers before an operation proceeds, and flight is often blocked by front lines, checkpoints, or closed borders.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_asymmetric_theaters, payer,
    powerless, biographical, trapped, regional).

% Soldiers of states that apply this doctrine, whose treatment if captured depends on protective norms their own state is eroding. Historical reciprocity shielded prisoners when both sides honored the categories; every public instance of a signatory state denying status or coercing detainees gives adversaries a license argument. They cannot influence the doctrine while serving under it and are wholly exposed once captured.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, captured_signatory_forces_reciprocity_exposed, payer,
    powerless, biographical, trapped, global).

% Uniformed lawyers and senior officials who argued inside the government that the proposed interrogation and detention policies violated treaty commitments the state had ratified and would produce unreliable intelligence. Their memoranda circulated through the building; the final policy overruled them, and several paid visible career costs. They remain members of the institution, bound by oath and pension, but stood outside the decision that counted.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, jag_officers_and_internal_legal_dissenters, excluded,
    organized, biographical, identity_locked, national).

% Mandated to visit places of detention and report on treatment. Access to certain facilities and programs was denied, delayed, or conditioned for years; findings were transmitted confidentially and did not alter operating rules. They publish recommendations that enter the diplomatic record but not the chain of command.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, icrc_and_un_monitoring_bodies, excluded,
    institutional, generational, constrained, global).

% Adjudicate habeas petitions, classification challenges, and treaty-scope disputes brought by detainees and advocacy organizations. Has at times restored procedural rights against the policy's design and at times deferred to security assertions. Issues rulings that reshape the doctrine's edges but administers nothing and depends on other organs for implementation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_and_supranational_judicial_review, observer,
    institutional, generational, analytical, national).

% Partner governments that hosted facilities, participated in transfer arrangements, and cite the doctrine when drafting their own wartime rules of engagement. Gain operational latitude and a measure of deniability through the arrangement. Each can drop the template in a single policy cycle without dismantling anything domestically, which is why their commitment runs shallow.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, allied_governments_importing_precedent, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, executive_war_powers_authority).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single operational doctrine for categorizing and handling detainees, setting collateral-damage thresholds, and authorizing interrogation across services, agencies, and partner governments. Gives individual operators predictable rules and legal cover so battlefield decisions do not require case-by-case liability calculus, and keeps multinational operations interoperable.
% TRANSFER_FUNCTION: Moves bodily security and liberty from detained suspects and civilians near hostilities to the state, converted into intelligence product, operational freedom, and precedential legal power. Moves legal and political risk away from senior decision-makers and onto detainees, onto operators' careers and consciences, and onto future captured service members through reciprocity erosion.
% ABSENT_VOICES: Detainees had no counsel, no franchise, and no channel during the period the core policies were designed. Civilian communities in the theaters had no seat anywhere in the process. ICRC and UN monitoring bodies were denied access to the facilities where the sharpest practices occurred, and the internal legal dissent that existed was overruled and sidelined rather than accommodated. Unanimity behind the doctrine was produced inside a room from which everyone it burdened had been removed.
% DISAPPEARANCE_RATIONALE: If the necessity-suspension framework vanished overnight, detention operations would reorganize around status determination and trial-or-release timelines, interrogation programs would revert to rapport-based methods already proven at scale by conventional interrogators, targeting approval chains would rebuild around the stricter civilian-immunity defaults, and allied governments would lose the template they import. Nothing in the physical world depends on the framework; the entire rearrangement is institutional.
% FOUNDING_PROBLEM: Note: see above; single founding_problem field authored once.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties, with the corroboration cutting against the reading's inference: the ICRC's confidential and leaked reporting on CIA detention, the Senate Select Committee on Intelligence study summary, the Boumediene majority opinion, testimony from retired senior military interrogators, and successive UN Special Rapporteur reports all attest that the categorical mismatch in asymmetric conflict is real. Those same sources dispute that the mismatch justifies suspending protections, documenting that non-coercive interrogation produced the actionable intelligence and that the categories were extended, not broken, by prior practice. No source outside the benefiting parties attests that the problem requires this reading's solution.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.79, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because the arrangement transfers liberty and bodily security wholesale from detainees and theater civilians while the transferred goods (intelligence, discretion) accrue to a single institutional seat; the rate decouples from any necessity demonstration — the Senate study and retired interrogators indicate the coercive increment added little reliable product. Suppression (0.68) is a raw structural property, unscaled by power or scope: habeas stripping, evidence classification, monitor-access denial, and internal dissent management are the load-bearing machinery. Theater (0.40) is substantive rather than decorative: the OLC memo corpus, the 'safe, legal, and ethical' reframings, and periodic compliance reviews perform legality while the operative rules run underneath; the theater series peaks where memo production peaked. Accessibility_collapse is 0.52 — compliant alternatives demonstrably exist (states have processed large irregular detentions under full Convention application, and rapport-based interrogation produced the actionable cases), so alternatives do not vanish on understanding; they are foreclosed politically, not technically. Resistance is 0.62: sustained, multi-front (courts partially winning, monitors publishing, insiders dissenting, publics oscillating) yet never displacing the core practices. The temporal series traces a ratchet-with-rollback rather than a cycle: sharp intensification to 2007-09, partial rollback (habeas restored, interrogation authorization rescinded), then slow re-hardening as detention persisted and collateral tolerance normalized. On coalition: the payer classes are prevented from coalition by design — isolation blocks detainee coordination, and theater civilians can only aggregate through the excluded monitor seats, which is itself part of the arrangement's structure.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setter seat the arrangement is prudential realism: a mature power declining to handicap itself against an opponent who ignores all rules. From the detainee and civilian seats the same structure is the administrative disappearance of their protected-person status. The operator seat straddles: it consumes the doctrine's legal cover while absorbing its residues. The judicial seat experiences the structure episodically, as a sequence of cases won and lost rather than a standing condition. Nothing in the authored claim resolves this; the engine computes the divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The executive seat sits near the full-beneficiary pole (collects product and discretion, designs around friction). Allied governments sit beneficiary-side with high exit mobility. Detainees and theater civilians sit at the full-target pole: trapped exit amplifies their effective position toward maximum. Captured signatory forces are structurally targets of a second order — they pay in a currency (future reciprocity) the arrangement spends now. One override is declared: for the organized power atom, derived d from the operators' beneficiary declaration alone would land near the subsidized end (~0.15), ignoring that the same operators bear prosecution exposure, documented moral injury, and reciprocity risk against their own captured comrades; 0.38 corrects the derivation toward their net position. The excluded seats (internal dissenters, monitors) carry no gain-flow either direction and contribute no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — categorical mismatch between Convention categories and irregular belligerents, plus time-pressured intelligence needs — is contested-live: real and ongoing, per corroborating sources outside the beneficiary set, but not established as requiring this reading's solution. Mandatrophy is therefore NOT declared resolved. The classification discipline cuts both ways here: reading the arrangement as pure coordination misses that its gains concentrate in one seat while its costs are distributed across people with no exit; reading it as pure extraction misses the doctrinal function that every historical military has independently sought from codified handling rules. The receipt surface sharpens the picture honestly against my own claim: gains name a single capturing seat and fixing is demonstrably cheap (interrogation authorization was reversed by one signature; habeas compliance followed a court order), which is the capture-adjacent cell — suggesting the hybrid is weighted toward its extractive face and drifting further as theater grows. If the identity frame broke — if operators and commanders internalized the documented unreliability of the coercive increment as professional consensus rather than outsider criticism — the coordination story would thin rapidly and the residual would classify as enforcement-wrapped extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the geneva_conventions_1949 kernel — the security_maximization_reading. How would instantiating a sibling reading change the structural data?',
    'Compare against the sibling files: the humanitarian_ceiling_reading authors a small victim set (violators of absolute floors) and low epsilon over the compliant-conduct arrangement; the conditional_reciprocity_reading makes victimhood contingent on adversary classification and introduces adversary conduct as a structural input this reading does not carry.',
    'Under the humanitarian ceiling, today''s detention and interrogation practices become the extraction itself with epsilon near maximum; under conditional reciprocity, part of what this story books as extraction is reattributed to adversary non-compliance. The classification of the same conduct flips across readings — the disagreement lives in the derogability premise, not in the facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Reading-indexed structure: sibling readings instantiate different constraints over the same conduct.').

omega_variable(
    coercive_interrogation_efficacy,
    'Does the coercive interrogation increment produce reliable, actionable intelligence beyond what rapport-based methods yield?',
    'Declassified program assessments — the Senate study, inspector-general reviews, contemporaneous FBI and military interrogator records — cross-checked against the operational trail of intelligence actually relied upon.',
    'If the increment is ineffective, the arrangement''s coordination payoff collapses and the residual is enforcement-wrapped extraction (snare-leaning); if effective, part of the measured extraction is the price of a functioning, if harsh, coordination mechanism (tangled-rope confirmation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_interrogation_efficacy, empirical, 'Whether the security justification for the coercive increment survives its own evidentiary record.').

omega_variable(
    civilian_death_attribution_shielding_vs_targeting,
    'Are civilian deaths in proximity-to-combatant strikes attributable to adversary shielding practices or to the targeting state''s threshold choices?',
    'Comparative casualty-recording methodology across theaters with different adversary embedding patterns, holding targeting doctrine constant; and within-theater comparison before and after threshold changes.',
    'If shielding explains the deaths, the victim ledger shifts toward the adversary''s account and the reading''s degraded-immunity output is partially exculpated; if threshold choice explains them, the civilian seat''s directionality hardens at the target pole and the arrangement''s extraction deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_death_attribution_shielding_vs_targeting, conceptual, 'Attribution dispute underlying the human-shields degradation of civilian immunity.').

omega_variable(
    reciprocity_erosion_contagion,
    'Does operating this doctrine measurably increase the risk to signatory states'' own captured forces?',
    'Longitudinal comparison of treatment of captured signatory personnel across conflicts before and after 2001, controlling for adversary type; adversary legal-justification texts citing the doctrine as precedent.',
    'If contagion is confirmed, captured_signatory_forces move from diffuse second-order payer toward primary victim, raising aggregate extraction and adding a constituency inside the military for restoring the ceiling; if not, their seat stays diffuse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_erosion_contagion, empirical, 'Whether the norm the arrangement spends down is one its own constituents were drawing on.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (legal barriers, classification, custody, access denial) or internalized (institutional conviction that necessity makes the practices correct)?',
    'Post-exit trajectory of former officials and operators: if suppression beliefs persist after leaving the structure — former officials defending practices their own documents undermine — the internalized share is large; the documented conversions of departing dissenters suggest the opposite for part of the population.',
    'If largely internalized, formal reform would leave the operative frame intact and the constraint would regenerate through training and promotion pipelines; if largely structural, removing the legal scaffolding would collapse the practice quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split behind the suppression scalar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t4, geneva_conventions_1949__security_maximization_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement_basis(gene_tr_t4, observed).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_1949__security_maximization_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement_basis(gene_tr_t8, observed).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_1949__security_maximization_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(gene_tr_t12, observed).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_1949__security_maximization_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(gene_tr_t16, observed).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(gene_tr_t20, observed).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_1949__security_maximization_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(gene_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t4, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 4, 0.74).
narrative_ontology:measurement_basis(gene_be_t4, observed).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 8, 0.8).
narrative_ontology:measurement_basis(gene_be_t8, observed).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 12, 0.73).
narrative_ontology:measurement_basis(gene_be_t12, observed).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement_basis(gene_be_t16, observed).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement_basis(gene_be_t20, observed).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement_basis(gene_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t4, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement_basis(gene_su_t4, observed).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement_basis(gene_su_t8, observed).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(gene_su_t12, observed).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(gene_su_t16, observed).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(gene_su_t20, observed).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(gene_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the Geneva Conventions.' The label covers at least three structurally distinct constraints — one per reading of the kernel. This member (security_maximization_reading) authors epsilon for the standing necessity-suspended arrangement as this reading operates it; the humanitarian_ceiling_reading authors epsilon over the same conduct from an absolute-floor frame (near-maximum extraction); the conditional_reciprocity_reading conditions victimhood on adversary compliance, producing a different victim set entirely. The readings differ on two structural elements: derogability of protections, and the criterion for protectable-person status. Family members are linked via affects_constraints; upstream/downstream citation traffic runs from whichever reading a government adopts to the targeting and detention sub-doctrines it licenses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

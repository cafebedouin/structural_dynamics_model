% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Dignity-Conditional Speech Protection Regime
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the speech_protection_kernel: the
 *   dignity reading, under which speech protection is a conditional grant —
 *   expression retains protection only insofar as it does not function as
 *   structural subordination of a target group. On this reading, group harm
 *   is recognized as distinct in kind from aggregated individual harm; hate
 *   speech and group libel fall outside protection; the warrant for
 *   protection is maintenance of equal civic dignity. The standing
 *   arrangement under contest — and therefore the epsilon referent — is this
 *   dignity-conditional regime itself, assessed by the reading's own lights:
 *   the burdens the regime imposes on speakers are weighed as this reading
 *   weighs them, not as an absolutist would. Sibling readings (absolutist,
 *   harm_threshold, marketplace, democratic_participation) are separate
 *   constraint files linked through network.affects_constraints; they are not
 *   described, hedged, or averaged here. Claim and metrics are independent
 *   authored facts: claimed_type is authored from structural belief
 *   (tangled_rope — a genuine coordination function combined with asymmetric
 *   extraction requiring active enforcement), while the metrics describe the
 *   regime's observed operation.
 *
 * KEY AGENTS:
 *   - - equality_adjudicators: Agenda-setter (institutional/identity_locked) — applies the subordination test, issues binding classifications
 *   - - members_of_targeted_groups: Primary beneficiary (moderate/constrained) — holds protected equal civic standing
 *   - - sanctioned_speakers: Primary target (moderate/constrained) — bears withdrawal of protection and sanctions
 *   - - boundary_adjacent_speakers: Secondary target (moderate/constrained) — bears deterrence chill without sanction
 *   - - general_deliberative_public: Dual-positioned beneficiary/payer (moderate/constrained) — gains the inclusive forum, funds enforcement
 *   - - civil_liberties_absolutists: Excluded objectors (organized/mobile) — hold the categorical premise outside the framework's terms
 *   - - human_rights_monitoring_bodies: Analytical observer (institutional/analytical) — reviews compliance, recommends broader coverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.56).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Dignity-Conditional Speech Protection Regime").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '3a79bb04-5ae6-4470-9109-10d93a737a6f').
narrative_ontology:cs_kernel_codification('3a79bb04-5ae6-4470-9109-10d93a737a6f', formalized).
narrative_ontology:cs_authority_grounding('3a79bb04-5ae6-4470-9109-10d93a737a6f', lineage).
narrative_ontology:cs_interpretation_layer_present('3a79bb04-5ae6-4470-9109-10d93a737a6f').
narrative_ontology:cs_reading_relation('3a79bb04-5ae6-4470-9109-10d93a737a6f', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('3a79bb04-5ae6-4470-9109-10d93a737a6f', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a79bb04-5ae6-4470-9109-10d93a737a6f', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a79bb04-5ae6-4470-9109-10d93a737a6f', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('3a79bb04-5ae6-4470-9109-10d93a737a6f', foundational, group_subordination_forfeits_protection).
narrative_ontology:cs_axiom_status(group_subordination_forfeits_protection, holdable).
narrative_ontology:cs_axiom_grounding('3a79bb04-5ae6-4470-9109-10d93a737a6f', group_subordination_forfeits_protection, deontological).
narrative_ontology:cs_axiom('3a79bb04-5ae6-4470-9109-10d93a737a6f', foundational, group_harm_structurally_distinct).
narrative_ontology:cs_axiom_status(group_harm_structurally_distinct, holdable).
narrative_ontology:cs_axiom_grounding('3a79bb04-5ae6-4470-9109-10d93a737a6f', group_harm_structurally_distinct, empirically_contingent).
narrative_ontology:cs_reference_frame('3a79bb04-5ae6-4470-9109-10d93a737a6f', dignity_conditioned_protection_baseline).
narrative_ontology:cs_drift_state('3a79bb04-5ae6-4470-9109-10d93a737a6f', contemporary_platform_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a79bb04-5ae6-4470-9109-10d93a737a6f', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, members_of_targeted_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, general_deliberative_public).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, sanctioned_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, boundary_adjacent_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, general_deliberative_public).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_harm_distinctness_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional courts, equality bodies, and prosecutors apply the subordination test: they decide which expressions count as functioning subordination of a group, issue binding classifications, and impose fines, prosecutions, or civil liability. Each newly recognized category enlarges their docket and authority. The equality mission constitutes these institutions; abandoning the frame would dissolve the body itself, so exit from the interpretive role is not available to them as organizations.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, equality_adjudicators, agenda_setter,
    institutional, generational, identity_locked, national).

% Racial, ethnic, religious, and gender minorities whose equal civic standing the regime protects. They gain a discourse environment in which their group is not publicly denigrated as a class, and with it access to media, politics, and public debate that denigration would close off. Individually they remain exposed to unclassified hostility, and their coverage depends on adjudicators recognizing their group as protected.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, members_of_targeted_groups, beneficiary,
    moderate, generational, constrained, national).

% Speakers whose expression is classified as subordinating: agitators, group-libel publishers, online harassers of protected classes. Protection is withdrawn retroactively from speech they may have believed lawful; they face fines, prosecution, or damages. Avoidance runs through silence, rephrasing, or leaving the jurisdiction; within it, the classification binds.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, sanctioned_speakers, payer,
    moderate, biographical, constrained, national).

% Academics, journalists, satirists, and debaters whose lawful speech skirts the subordination predicate. They are rarely sanctioned but rationally discount the safety of their expression, softening coverage of migration, religion, or group-differentiated statistics. Their exit is self-censorship within the regime, or relocating their audience outside it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, boundary_adjacent_speakers, payer,
    moderate, biographical, constrained, national).

% The citizen body sharing the regulated discourse space. It gains a forum kept open to members of all groups and loses a margin of expressive liberty along with the assurance that unpopular views will find protection; it also funds the enforcement apparatus through taxation. Its members cannot exit the discourse environment short of emigration.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, general_deliberative_public, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, general_deliberative_public, payer).

% Civil-liberties organizations and jurists holding that no speech category warrants withdrawal of protection. They litigate individual cases and publish critiques, but the framework's constitutive premise — that subordinating speech can forfeit protection — is not a position they can hold inside the adjudicative conversation; they appear only as objectors to outcomes, never as co-authors of the standard.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, civil_liberties_absolutists, excluded,
    organized, generational, mobile, national).

% Treaty committees and anti-discrimination monitoring bodies review state compliance with dignity-protective obligations, take testimony from all seats, issue recommendations, and press jurisdictions toward broader coverage of protected groups. They decide nothing binding domestically and bear none of the regime's costs.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, equality_adjudicators).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a floor of mutual recognition in public discourse so that members of all groups can participate as civic equals; prevents discourse dynamics from re-enacting the group hierarchies that would drive target-group members out of public life. Stated without evaluation of whether the price paid is worth the floor.
% TRANSFER_FUNCTION: Moves expressive liberty from speakers whose expression functions as subordination (and, via deterrence, from boundary-adjacent speakers) toward the equal civic standing of target-group members; moves classification authority to courts, equality bodies, and prosecutors; moves enforcement costs to the general taxpayer.
% ABSENT_VOICES: Civil-liberties absolutists hold that no speech category warrants withdrawal of protection; they litigate and publish but cannot co-author the standard — their constitutive premise has no seat in the adjudicative conversation. Accused speakers likewise had no seat where the subordination taxonomy was built: the categories were drafted by legislatures, treaty bodies, and courts, not by those whose speech they classify.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, hate-speech statutes, equality tribunals, and platform dignity protocols would lose their warrant; target-group participation patterns in media and politics would shift as uncensored group-denigration returned; ongoing prosecutions and content-moderation pipelines would unwind; jurisdictions would face pressure to renegotiate treaty commitments. Visible arrangements depend on it.
% FOUNDING_PROBLEM: Post-1945 constitution-makers faced formal speech freedom coexisting with mass-scale racist and fascist agitation that had preceded genocide: how to keep speech free while preventing discourse from functioning again as an instrument of group subordination and exclusion from civic life.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historical record assembled by historians of fascist and segregationist propaganda; the drafting history of the German Basic Law (1949) and of ICERD (1965), ratified across Cold-War bloc lines; and continuing documentation by race-equality monitoring bodies. Signal-quality note: the strongest external attestations come from parties who accept the founding problem while disputing this reading's remedy — corroboration of the problem, not of the solution.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.56: the regime withdraws protection retroactively from classified speech, concentrates an indeterminate predicate ('functions as structural subordination') in adjudicator hands, and extends deterrence to lawful boundary-adjacent speech; the reading's own lights nonetheless count a substantial share of that burden as the price of the coordination the regime exists to provide, so epsilon sits mid-range rather than snare-grade. Suppression 0.62 is authored as a raw, unscaled structural property: persistence requires continuously operated enforcement machinery (statutes, prosecutors, equality tribunals, and latterly platform protocols), not participant preference — only extractiveness is scaled by directionality and scope downstream. Theater 0.21: adjudication and sanction are predominantly functional; a minor share is reputational compliance (codes adopted for standing rather than enforcement). Accessibility_collapse 0.40: speakers retain partial exits — rephrasing, jurisdictional arbitrage, pseudonymity — so alternatives degrade but do not vanish. Resistance 0.60: sustained absolutist litigation, scholarly opposition, and periodic political backlash; payer resistance stays below revolt levels because payers are diffuse and ideologically heterogeneous, which weakens coalition formation. The temporal series run on ONE shared grid (t = 0,15,30,45,60,75, approximately 1950-2025 at fifteen-year steps) with every tracked metric authored at every point; suppression_requirement is tracked because enforcement-capacity build-out (the ICERD ratification wave, tribunal creation, extension of scope to online speech) is the interval's defining dynamic. Final series values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the adjudicator seat the regime is the minimal precondition of a usable public sphere — a coordination instrument administered in good faith. From the sanctioned-speaker seat the same structure is arbitrary deprivation: a tribunal applying an indeterminate predicate to speech believed lawful. From the beneficiary seat it is the difference between participating in public life and exiting it. From the excluded absolutist seat it is a category mistake — protection that can be lost was never protection. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Members_of_targeted_groups and the deliberative public sit near the beneficiary pole (d near 0): the regime subsidizes their equal standing. Sanctioned and boundary-adjacent speakers sit near the target pole (d approaching 1): they supply the withdrawn liberty, and constrained exits keep them from arbitrage relief. Equality_adjudicators occupy a distinctive middle-low position: they collect authority rather than liberty, and their identity_locked exit binds them to maintenance — the equality mission constitutes the institution, so abandoning the frame would dissolve the body (an institutional-identity fusion, not career path dependence). Human_rights_monitoring_bodies hold an analytical seat with no extraction exposure. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already differentiate the seats, and the stakeholder set's power atoms are too homogeneous (mostly moderate) for per-atom overrides to add resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both overcalls. Reading the regime as pure rope ignores that speakers bear concentrated, retroactive losses and that adjudicators hold discretionary standard-setting power — the asymmetry the tangled_rope gate requires naming. Reading it as pure snare ignores the genuine coordination function: absent the dignity floor, discourse dynamics exclude whole classes from public life, which is exactly the collective-action failure the regime solves. The founding problem (discourse re-enacting group hierarchy) remains live and externally corroborated, so the mandate has not outlived its function: no mandatrophy_resolved declaration, no sunset clause (not scaffold), no atrophied performance (not piton — theater_ratio 0.21 with functional enforcement, and adjudicators plainly profit in authority from maintaining it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which structural elements of the speech-protection kernel does the dignity reading fix, and what would each sibling reading change?',
    'Compare the compiled sibling stories: victim sets (none vs individual-harm-bearers vs subordinated groups and chilled speakers), enforcement structure, and per-seat classifications. The spread of epsilon across readings measures the kernel contest, not measurement error.',
    'If sibling stories compute materially different types from the same topic language, the kernel decomposes as designed; if they converge, the reading distinctions are rhetorical rather than structural and the family collapses toward one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-indexed identity of this constraint within the speech_protection_kernel family.').

omega_variable(
    subordination_predicate_indeterminacy,
    'Is the predicate ''functions as structural subordination of a target group'' determinate enough to bound enforcement, or does its vagueness delegate standard-setting to adjudicators case by case?',
    'Track appellate reversal rates, doctrinal test evolution (incitement-to-group-hatred thresholds, group-libel boundaries), and legislative narrowing attempts across dignity-regime jurisdictions.',
    'If indeterminate, effective extraction on boundary-adjacent speakers exceeds the authored epsilon and the regime drifts toward snare-flavored operation at the payer seats; if determinate, the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_predicate_indeterminacy, empirical, 'Determinacy of the subordination predicate under adjudication.').

omega_variable(
    chilling_effect_extent,
    'How far does deterrence extend beyond the sanctioned categories into lawful speech?',
    'Comparative survey and experimental evidence on self-censorship among academics, journalists, and satirists in dignity-regime versus categorical-protection jurisdictions.',
    'Greater chilling raises the payer seats'' effective burden and supports the tangled_rope rather than rope assessment; negligible chilling would support reclassification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_extent, empirical, 'Magnitude of deterrence spillover onto lawful speech.').

omega_variable(
    hybrid_framework_coherence,
    'Can any single legal framework stably hold the dignity reading''s conditional premise alongside the absolutist premise that listener harm never restricts — that is, is the forecloses edge to absolutist_reading stable under hybrid institutional design?',
    'Examine jurisdictions blending categorical protection with narrow dignity exceptions (categorical political-speech protection plus incitement carve-outs) for stable operation without doctrinal incoherence.',
    'If hybrids are stable, the foreclosure relation downgrades to coexists_with and the kernel topology changes; if they oscillate or collapse into one premise, foreclosure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_framework_coherence, conceptual, 'Stability of the foreclosure edge between the dignity and absolutist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__dignity_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__dignity_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(spee_tr_t30, observed).
narrative_ontology:measurement(spee_tr_t45, speech_protection_kernel__dignity_reading, theater_ratio, 45, 0.17).
narrative_ontology:measurement_basis(spee_tr_t45, observed).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__dignity_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(spee_tr_t60, observed).
narrative_ontology:measurement(spee_tr_t75, speech_protection_kernel__dignity_reading, theater_ratio, 75, 0.21).
narrative_ontology:measurement_basis(spee_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__dignity_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__dignity_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(spee_be_t30, observed).
narrative_ontology:measurement(spee_be_t45, speech_protection_kernel__dignity_reading, base_extractiveness, 45, 0.49).
narrative_ontology:measurement_basis(spee_be_t45, observed).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__dignity_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement_basis(spee_be_t60, observed).
narrative_ontology:measurement(spee_be_t75, speech_protection_kernel__dignity_reading, base_extractiveness, 75, 0.56).
narrative_ontology:measurement_basis(spee_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__dignity_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__dignity_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(spee_su_t30, observed).
narrative_ontology:measurement(spee_su_t45, speech_protection_kernel__dignity_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement_basis(spee_su_t45, observed).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__dignity_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(spee_su_t60, observed).
narrative_ontology:measurement(spee_su_t75, speech_protection_kernel__dignity_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement_basis(spee_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% 'Freedom of speech' is a colloquial label covering at least five structurally distinct constraints; this file instantiates only the dignity reading, whose distinctive victim set comprises speakers classified as subordinating plus boundary-adjacent chilled speakers, and whose beneficiary set centers on members of target groups. Each sibling is a separate file with its own epsilon, beneficiaries, and victims. Coupling: the dignity reading's group-harm distinctness thesis is cited by harm_threshold proponents seeking to widen 'harm', and its protections are increasingly justified in democratic_participation terms — upstream claims feeding downstream reframings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

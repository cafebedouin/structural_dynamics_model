% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Restrictive Sovereignty Reading of the Refugee Convention
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates the restrictive-sovereignty reading of the 1951
 *   Refugee Convention text: the Convention is read as a minimum floor of
 *   protection that preserves maximum sovereign discretion over admission.
 *   Under this reading, 'well-founded fear' demands individualized proof of
 *   persecution (severity of generalized danger alone does not qualify), and
 *   'particular social group' is confined to immutable characteristics
 *   coupled with a demonstrable state nexus (state persecution, or state
 *   unwillingness specifically, not mere inability to control non-state
 *   actors). This is a distinct constraint from the
 *   expansive_humanitarian_reading and procedural_integrity_reading siblings
 *   — those are separate files with their own ε, victim sets, and
 *   classifications, linked here only through the shared kernel and network
 *   edges. The narrow eligibility screen this reading produces has hardened
 *   over four decades from a textual interpretive choice into an entrenched
 *   administrative and commercial apparatus (offshore processing contracts,
 *   dedicated denial infrastructure) that now has its own institutional
 *   stakeholders independent of the original interpretive question.
 *
 * KEY AGENTS:
 *   - destination_state_governments: agenda_setter (institutional/arbitrage) — author and apply the restrictive standard
 *   - border_enforcement_agencies: beneficiary/agenda_setter (institutional/arbitrage) — administer high-throughput denial under the standard
 *   - offshore_processing_contractors: beneficiary (organized/arbitrage) — commercial interest riding on offshore-transfer permissibility
 *   - generalized_violence_asylum_seekers, non_state_persecution_claimants, gender_based_claimants_without_immutability_proof, detained_offshore_transferees: payers (powerless/trapped) — excluded or detained under the narrow eligibility screen
 *   - unhcr_and_treaty_monitoring_bodies: excluded (organized/analytical) — advisory but non-binding
 *   - asylum_appellate_courts: observer (institutional/analytical) — adjudicate individual cases within the standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.72).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Restrictive Sovereignty Reading of the Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'c5199b78-ea55-43e4-b3c7-d5c0d091ee25').
narrative_ontology:cs_kernel_codification('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', fixed_text).
narrative_ontology:cs_authority_grounding('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', practice).
narrative_ontology:cs_interpretation_layer_present('c5199b78-ea55-43e4-b3c7-d5c0d091ee25').
narrative_ontology:cs_reading_relation('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', foundational, sovereign_admission_discretion_is_primary).
narrative_ontology:cs_axiom_status(sovereign_admission_discretion_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', sovereign_admission_discretion_is_primary, conventional).
narrative_ontology:cs_axiom('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', foundational, persecution_requires_individualized_state_linked_proof).
narrative_ontology:cs_axiom_status(persecution_requires_individualized_state_linked_proof, holdable).
narrative_ontology:cs_axiom_grounding('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', persecution_requires_individualized_state_linked_proof, empirically_contingent).
narrative_ontology:cs_reference_frame('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', postwar_bounded_protection_floor).
narrative_ontology:cs_drift_state('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', contemporary_mass_displacement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5199b78-ea55-43e4-b3c7-d5c0d091ee25', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_contractors).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, generalized_violence_asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, gender_based_claimants_without_immutability_proof).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, detained_offshore_transferees).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, state_sovereignty_over_admission_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, textualist_treaty_interpretation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt the restrictive reading of the Convention text to justify narrow eligibility screening, offshore processing arrangements, and rejection of claims not resting on individualized, state-linked persecution tied to an immutable characteristic. Frame the reading as fidelity to the treaty's original text rather than as policy choice, and control the domestic legal apparatus that applies it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate admissibility screening and interdiction under the restrictive standard, which reduces caseload volume and gives them clear textual grounds to deny or redirect claims that would otherwise require substantive hearings. Their institutional mandate and budget depend on high rejection-at-the-border throughput.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, agenda_setter).

% Operate detention and processing facilities in third countries under contracts that exist only because the restrictive reading holds offshore transfer to be Convention-compliant. Revenue scales with the number and duration of detentions; the reading's persistence is their business model's precondition.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_contractors, beneficiary,
    organized, biographical, arbitrage, regional).

% Flee civil war, gang control, or collapsed state authority without being able to point to an individualized persecutor targeting them specifically. Under this reading their claims fail at the threshold because the harm is diffuse rather than individualized, regardless of how severe or life-threatening it is. They have no forum in which the severity of the danger substitutes for the missing individualization proof.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, generalized_violence_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Face persecution from cartels, militias, or family/community actors the state has not authorized, tolerated with documented specificity, or is deemed merely 'unable' (not 'unwilling') to control. The state-awareness requirement in this reading forces them to prove a state nexus that frequently does not exist in the form demanded, closing off claims that would succeed under a broader reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_claimants, payer,
    powerless, immediate, trapped, global).

% Assert persecution tied to gender roles, forced marriage resistance, or sexual orientation performance that adjudicators under this reading treat as insufficiently 'immutable' or insufficiently particularized as a cognizable social group. Their claims are denied not on credibility grounds but on the categorical scope of 'particular social group' as this reading defines it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, gender_based_claimants_without_immutability_proof, payer,
    powerless, immediate, trapped, global).

% Are physically removed to third-country processing centers permitted under this reading's tolerance for offshore arrangements, held for extended periods with limited access to counsel, appeal infrastructure, or the destination state's own courts. Their remoteness from the deciding jurisdiction is itself a structural barrier the reading does not treat as a Convention violation.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, detained_offshore_transferees, payer,
    powerless, biographical, trapped, regional).

% Publish interpretive guidance favoring broader readings of 'well-founded fear' and 'particular social group,' but have no binding enforcement authority over state courts adopting the restrictive reading. Their commentary is cited in dissents and academic literature but does not bind the domestic tribunals applying this reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr_and_treaty_monitoring_bodies, excluded,
    organized, generational, analytical, global).

% Review individual denials and are the forum where the restrictive reading is tested against competing interpretations case by case. Some panels entrench the restrictive reading through precedent; others carve narrow exceptions. Their rulings shape whether the reading hardens or softens over time without themselves controlling the treaty text.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_appellate_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives destination states a stable, predictable, judicially administrable eligibility standard that can be applied consistently across large volumes of claims without requiring open-ended case-by-case moral judgment about the severity of generalized danger.
% TRANSFER_FUNCTION: Moves the cost of unresolved danger from destination states (who would otherwise bear resettlement, adjudication, and integration costs) onto asylum seekers whose harm does not fit the individualized-persecution template, and moves detention/processing costs and risks onto third-country contractors' detainees rather than the destination state's own territory and courts.
% ABSENT_VOICES: UNHCR and treaty monitoring bodies articulate the broader reading in guidance and advisory opinions but hold no binding authority over the domestic courts and executive agencies that actually apply this reading; source-country civil society and displaced populations themselves have no standing in the destination state's interpretive process at all.
% DISAPPEARANCE_RATIONALE: If the restrictive reading were displaced by the expansive or procedural readings, admissibility screening would loosen substantially, offshore processing arrangements would face renewed legal challenge as Convention violations, caseloads and resettlement obligations for destination states would expand sharply, and the contractor industry built around offshore detention would lose its legal predicate.
% FOUNDING_PROBLEM: The 1951 Convention was drafted to give states a workable, bounded commitment to protect a defined class of persecuted individuals (initially European, post-WWII) without obligating unlimited admission of anyone fleeing any hardship — a floor of protection paired with sovereign control over who qualifies.
% FOUNDING_PROBLEM_CORROBORATION: Destination-state governments and their courts attest the founding problem remains live and correctly bounded by the text's individualized-persecution language. UNHCR, refugee law scholars outside government service, and dissenting appellate judges attest that the founding problem has evolved past the drafters' postwar frame — mass displacement from generalized violence and non-state actors is now the modal case — and that the restrictive reading persists less to honor original intent than to minimize destination-state obligation under cover of textualism.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (rising from 0.45) because the reading's function — narrowing eligibility to reduce state obligation — has intensified as displacement patterns increasingly involve generalized violence and non-state actors that the standard was never built to admit; the gap between who needs protection and who qualifies has widened over the interval even though the textual standard itself has not changed. Suppression at 0.72 reflects that the reading depends on active enforcement machinery (admissibility screening, offshore detention, denial precedent) rather than voluntary acceptance by those it excludes. Theater ratio at 0.45 captures that a meaningful share of the apparatus — individualized hearings, appellate review — retains real adjudicative function even as an increasing share serves to perform textual fidelity while producing predetermined denial outcomes for categories of claim the standard was designed to exclude. All three time series share the single 0-40 grid.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat this reading is principled textualism preserving sovereign prerogative recognized by the treaty itself. From the payer seats — especially claimants fleeing generalized violence or non-state persecution — the same text-based standard operates as a categorical exclusion mechanism that treats the severity of their danger as legally irrelevant. The engine computes this divergence from the structural power/exit data; it is not resolved by declaring one seat correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Destination state governments and border enforcement agencies sit at the beneficiary end: they set and administer the standard and it reduces their obligations. Offshore processing contractors are a derived beneficiary — their business model has no legal predicate without the reading's tolerance for extraterritorial transfer. The four payer groups are powerless and trapped: they cannot exit the jurisdiction whose courts apply the standard, cannot individually contest the interpretive framework, and bear the full cost of falling outside its narrow eligibility window regardless of the severity of the danger they fled. UNHCR's exclusion from binding authority means the interpretive contest is resolved entirely within destination-state institutions that also benefit from the restrictive answer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a bounded, administrable commitment to protect a defined persecuted class without unlimited admission — remains partially live (states retain legitimate interests in orderly admission) but the reading's current operation increasingly serves a different function: minimizing obligation in the face of a displacement landscape (generalized violence, non-state persecution, gender-based claims) the original drafters did not contemplate and that the restrictive interpretation was not logically compelled to exclude. This is why the classification is tangled_rope rather than snare or mountain: there IS a genuine coordination function (predictable, administrable eligibility screening prevents arbitrary case-by-case discretion) coexisting with asymmetric extraction (categories of genuine danger are excluded specifically because inclusion would raise destination-state cost). Treating this as pure mountain (settled textual meaning) would launder the extraction; treating it as pure snare would erase the real coordination value of a bounded, predictable standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textualism_vs_instrumentalism,
    'Is the restrictive reading a faithful application of the Convention drafters'' original textual intent, or a instrumentally-selected interpretation favored because it minimizes destination-state obligation?',
    'Comparative analysis of travaux préparatoires against contemporary state practice; examination of whether states adopting the restrictive reading do so consistently across all treaty provisions or selectively where it reduces cost.',
    'If genuinely originalist, the reading has independent interpretive legitimacy separable from its cost-minimizing effect. If instrumentally selected, the textualist framing functions as cover for a policy preference, strengthening the case for classifying the beneficiary capture as the reading''s primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualism_vs_instrumentalism, conceptual, 'Whether restrictive textualism is genuine interpretive method or outcome-driven selection.').

omega_variable(
    state_nexus_requirement_defensibility,
    'Is the state-awareness/state-nexus requirement for ''particular social group'' a defensible reading of the treaty''s structure, or does it read a limitation into the text that the drafters did not intend and that modern displacement patterns render obsolete?',
    'Comparative jurisprudence across jurisdictions applying different nexus standards; tracking outcome divergence for factually similar non-state persecution claims across restrictive vs. broader-reading jurisdictions.',
    'If the nexus requirement is textually compelled, exclusion of non-state persecution claimants is a structural feature of the Convention itself, not of this reading specifically. If not compelled, the exclusion is a reading-specific policy choice that could be revised without amending the treaty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_nexus_requirement_defensibility, empirical, 'Whether the state-nexus limitation is textually required or reading-specific.').

omega_variable(
    committer_framing_alternative,
    'Could this reading instead be framed as prioritizing the kernel''s authority-preservation function (states retain final say over admission) over its protection-extension function (broadest possible coverage of the persecuted), rather than as a substantive interpretation of ''well-founded fear'' and ''particular social group'' specifically?',
    'N/A — this is a framing-level ambiguity, not an empirically resolvable question. Documented per the CS-framing under-determination guidance: the obvious framing treats this as a textual-interpretation dispute (what do the words mean); a less obvious framing treats it as a background dispute about which of the treaty''s two functions (protection floor vs. sovereignty preservation) is primary, with the textual interpretation following from that prior choice.',
    'Under the textual-interpretation framing, this reading is one plausible construction among several defensible textual readings. Under the functional-priority framing, this reading is better understood as a sovereignty-preservation commitment that recruits textual argument in its service — which would strengthen the tangled_rope classification by making the beneficiary structure prior to, rather than incidental to, the interpretive method.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether this is fundamentally a textual dispute or a prior functional-priority dispute expressed through textual argument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(refu_tr_t8, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(refu_tr_t16, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(refu_tr_t24, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(refu_tr_t32, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(refu_tr_t40, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(refu_be_t8, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(refu_be_t16, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(refu_be_t24, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(refu_be_t32, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(refu_be_t40, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(refu_su_t8, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(refu_su_t16, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(refu_su_t24, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(refu_su_t32, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(refu_su_t40, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__restrictive_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the Refugee Convention's protection standard' per the ε-invariance principle. Each sibling reading (restrictive_sovereignty, expansive_humanitarian, procedural_integrity) instantiates a structurally distinct constraint from the same kernel text, with its own ε, beneficiary/victim structure, and classification. This reading is authored as tangled_rope (genuine administrability coordination + asymmetric extraction against categories of excluded claimants); the expansive_humanitarian sibling is expected to author a lower ε and a broader victim/beneficiary inversion; the procedural_integrity sibling is expected to decouple ε from substantive outcome entirely, focusing instead on process-fairness metrics. All three should be read as reading-indexed values over the same referent (the standing restrictive/contested arrangement), not as convergent measurements of one true ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

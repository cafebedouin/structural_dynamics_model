% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading of the Anthropological Record (Divine Creation / Designed Complexity)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested 'anthropological record'
 *   kernel: the creationist reading, which holds that the fossil, geological,
 *   and genetic record either reveals discrete divine creation events
 *   compatible with a scriptural timeline or exhibits designed complexity
 *   inconsistent with unguided materialist processes. This is not a story
 *   about the debate between readings — it is a clean structural account of
 *   how the creationist reading operates as an institutional arrangement:
 *   what it coordinates (community cohesion, doctrinal defense), who
 *   administers it, and who pays for its maintenance. The naturalist and
 *   indigenous-epistemology readings are separate constraints (see network
 *   links); this file does not average over them or hedge its epsilon against
 *   them.
 *
 * KEY AGENTS:
 *   - creationist_institutional_leadership: agenda_setter (institutional/arbitrage) — administers doctrine and curricula, collects revenue
 *   - affiliated_educational_publishers: beneficiary (organized/mobile) — monetizes required curricular content
 *   - young_earth_dissenting_scientists_within_movement: payer (moderate/constrained) — bears career and community cost of internal doubt
 *   - congregant_children_in_affiliated_schools: payer (powerless/trapped) — taught the reading as settled without consent
 *   - credentialed_paleoanthropologists_excluded_from_dialogue: excluded (institutional/analytical) — expertise structurally kept out of the room
 *   - lay_congregants_and_parents: beneficiary/payer (moderate/constrained) — gain coherence, bear social and financial cost
 *   - secular_science_education_boards: observer (institutional/analytical) — governs adjacent public curricula
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.58).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.71).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of the Anthropological Record (Divine Creation / Designed Complexity)").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '551fd2ba-22c8-423b-a747-0b4d0c455429').
narrative_ontology:cs_kernel_codification('551fd2ba-22c8-423b-a747-0b4d0c455429', fixed_text).
narrative_ontology:cs_authority_grounding('551fd2ba-22c8-423b-a747-0b4d0c455429', lineage).
narrative_ontology:cs_interpretation_layer_present('551fd2ba-22c8-423b-a747-0b4d0c455429').
narrative_ontology:cs_reading_relation('551fd2ba-22c8-423b-a747-0b4d0c455429', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('551fd2ba-22c8-423b-a747-0b4d0c455429', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('551fd2ba-22c8-423b-a747-0b4d0c455429', foundational, scripture_as_inerrant_historical_record).
narrative_ontology:cs_axiom_status(scripture_as_inerrant_historical_record, holdable).
narrative_ontology:cs_axiom_grounding('551fd2ba-22c8-423b-a747-0b4d0c455429', scripture_as_inerrant_historical_record, theological).
narrative_ontology:cs_axiom('551fd2ba-22c8-423b-a747-0b4d0c455429', foundational, designed_complexity_requires_intentional_agent).
narrative_ontology:cs_axiom_status(designed_complexity_requires_intentional_agent, holdable).
narrative_ontology:cs_axiom_grounding('551fd2ba-22c8-423b-a747-0b4d0c455429', designed_complexity_requires_intentional_agent, empirically_contingent).
narrative_ontology:cs_axiom('551fd2ba-22c8-423b-a747-0b4d0c455429', secondary, credentialed_science_subordinate_to_doctrinal_authority).
narrative_ontology:cs_axiom_status(credentialed_science_subordinate_to_doctrinal_authority, holdable).
narrative_ontology:cs_axiom_grounding('551fd2ba-22c8-423b-a747-0b4d0c455429', credentialed_science_subordinate_to_doctrinal_authority, conventional).
narrative_ontology:cs_reference_frame('551fd2ba-22c8-423b-a747-0b4d0c455429', scriptural_inerrancy_literal_chronology).
narrative_ontology:cs_drift_state('551fd2ba-22c8-423b-a747-0b4d0c455429', post_genomic_and_radiometric_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('551fd2ba-22c8-423b-a747-0b4d0c455429', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_institutional_leadership).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, affiliated_educational_publishers).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, young_earth_dissenting_scientists_within_movement).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, congregant_children_in_affiliated_schools).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, credentialed_paleoanthropologists_excluded_from_dialogue).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, lay_congregants_and_parents).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, lay_congregants_and_parents).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, scriptural_literalism).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, young_earth_or_designed_complexity_chronology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets doctrinal curricula, certifies affiliated schools and museums, and adjudicates which readings of the fossil and genetic record count as acceptable within the movement's institutions. Collects tuition, donations, and speaking/publishing revenue tied to defending the reading; controls exit for employees and affiliated scientists through statement-of-faith requirements.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, creationist_institutional_leadership, beneficiary).

% Produce and sell curricula, textbooks, and museum exhibits built on the creationist reading. Revenue depends on continued institutional demand for materials that treat the reading as settled; can pivot content if institutional leadership's requirements shift, giving them more exit than rank-and-file adherents.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, affiliated_educational_publishers, beneficiary,
    organized, biographical, mobile, national).

% Hold credentials in relevant fields and privately or publicly note tensions between the record and the required chronology. Employment at affiliated institutions is conditioned on public adherence to the reading via signed statements of faith; dissent risks termination, community ostracism, and loss of professional community built around the movement.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, young_earth_dissenting_scientists_within_movement, payer,
    moderate, biographical, constrained, national).

% Are taught the creationist reading as settled fact in schools and homeschool curricula chosen by parents and church leadership. Have no say in curriculum design and limited exposure to competing readings until adulthood; later encounters with the broader scientific consensus can produce acute credibility crises regarding both science and faith.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, congregant_children_in_affiliated_schools, payer,
    powerless, biographical, trapped, local).

% Would present dated fossil, radiometric, and genetic evidence for materialist human origins, but are structurally excluded from adjudicating truth claims within creationist institutions — their expertise is treated as either irrelevant to or actively opposed to the movement's founding commitments, so their objections never enter the decision-making room.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, credentialed_paleoanthropologists_excluded_from_dialogue, excluded,
    institutional, generational, analytical, global).

% Receive a coherent, community-reinforcing account of origins that integrates with their broader faith commitments and social belonging. Also bear costs: tithes and tuition fund the institutional apparatus, and social costs of questioning the reading (ostracism, loss of community standing) are real, making genuine reconsideration expensive.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, lay_congregants_and_parents, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, lay_congregants_and_parents, payer).

% Set standards for public science curricula and periodically litigate or legislate over whether creationist material can be taught alongside or instead of the naturalist reading in public schools; observe but do not control what happens inside private/religious institutions.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, secular_science_education_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, creationist_institutional_leadership).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, internally consistent account of human origins that integrates with scriptural authority, giving a religious community a unified epistemic framework, common curriculum, and defense against perceived threats to doctrinal coherence from secular science.
% TRANSFER_FUNCTION: Moves intellectual deference, tuition and donation revenue, and career conformity from congregants, students, and affiliated professionals toward institutional leadership and publishers who administer and monetize the doctrinally-required reading; moves credibility away from dissenting scientists and excluded external experts.
% ABSENT_VOICES: Credentialed paleoanthropologists and geneticists whose evidence would complicate the required chronology are not invited into doctrinal decision-making; internal dissenting scientists are present but structurally muzzled by employment conditions.
% DISAPPEARANCE_RATIONALE: If the creationist reading's institutional enforcement vanished overnight, affiliated schools would lose their curricular mandate, statement-of-faith employment conditions would lose their doctrinal justification, publishers would need to retool products, and many congregants would face open renegotiation of how faith and the fossil/genetic record relate — a substantial institutional and social rearrangement, not a null event.
% FOUNDING_PROBLEM: Perceived threat to scriptural authority and community cohesion posed by 19th-20th century geological and evolutionary science, and a desire to provide believers with an intellectually defensible alternative account of human origins consistent with their theology.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests the problem (defending scriptural authority against materialist encroachment) remains fully live. Historians of science and sociologists of religion studying the movement from outside it, along with some internal dissenting scientists, attest that the empirical justification for the specific chronology has become harder to sustain over the interval even as institutional demand for the reading persists — suggesting institutional self-perpetuation now runs ahead of the founding apologetic problem.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that beyond the genuine coordination function (a coherent worldview for believers), the arrangement also extracts tuition, donation revenue, and professional conformity from people with limited ability to contest the doctrinal terms. Suppression is high (0.71) because active enforcement mechanisms — statements of faith, employment conditionality, curricular gatekeeping — are required to keep the reading intact against countervailing evidence encountered by adherents in wider society. Theater ratio (0.42) captures that a meaningful share of institutional activity (apologetics conferences, museum exhibits, debate performances) functions as legitimacy display rather than genuine evidentiary engagement. Accessibility collapse (0.6) is moderate-high: once inside affiliated institutions, alternative readings are effectively unavailable, though the broader society retains open access to competing accounts. Resistance (0.68) is substantial: internal dissenters, excluded scientists, and public education boards all actively contest the reading's claims and its institutional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this reading is a coordination achievement: a stable, multi-generational worldview that holds a community together against perceived corrosive materialism. From the payer seats — dissenting internal scientists, trapped children, congregants funding an apparatus with limited internal contestability — the same structure operates as enforced conformity requiring active suppression of competing evidence and internal doubt. The engine computes this divergence from the structural roles and exit options; it is not asserted directly.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership sits near the full-beneficiary end: it sets terms, collects resources, and has arbitrage-grade exit (can adapt doctrine, move between institutional roles). Publishers are moderate beneficiaries with genuine market mobility. Dissenting internal scientists and trapped children sit near the target end: their exit options (constrained, trapped) are the mechanism by which extraction becomes effective rather than merely nominal. Excluded external scientists are outside the extraction relation entirely — they are not paying into the arrangement, they are locked out of adjudicating it, which is a distinct structural harm (epistemic exclusion) captured via the excluded role and absent_voices rather than via victim/beneficiary framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — defending scriptural authority against a perceived existential threat from 19th-century geology and Darwinian evolution — is contested as to whether it remains live. Institutional leadership insists the apologetic function is as necessary as ever; external historians of science and some internal dissenters increasingly view the empirical case as harder to sustain even as institutional demand (jobs, curricula, donor bases) has grown independent of its evidentiary strength. This is the tangled_rope signature precisely: real coordination benefit (community cohesion, meaning-making) persists alongside asymmetric extraction (revenue, conformity, epistemic foreclosure) that requires active enforcement to maintain — treating it as pure extraction would erase the genuine psychological and communal good it provides many adherents; treating it as pure coordination would erase the costs borne by trapped and dissenting parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_creationist,
    'Is the creationist reading of the anthropological record a live, internally coherent epistemic framework independently defensible on its own evidentiary terms, or is it primarily sustained by institutional path-dependency and community-cohesion incentives that have outrun its original apologetic justification?',
    'Track whether creationist institutions engage in good-faith, falsifiable predictions tested against new fossil/genetic discoveries versus post-hoc doctrinal accommodation; track career outcomes for internal dissenters as a proxy for genuine openness to revision.',
    'If primarily institutionally self-sustaining, the tangled_rope classification is well-supported and the coordination component is largely retrospective community-cohesion rather than live truth-seeking. If genuinely open to falsification and revision, the classification should shift toward rope with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_creationist, conceptual, 'Whether the creationist reading is evidentiarily live or institutionally self-perpetuating.').

omega_variable(
    sibling_reading_foreclosure_structure,
    'Where exactly is the disagreement located between the creationist, naturalist, and indigenous-epistemology readings — is it a disagreement about which evidence counts (epistemic method), about the causal mechanism of human origins (theological vs. materialist), or about the appropriate authority for adjudication (credentialed science vs. scriptural authority vs. oral tradition)?',
    'Structural comparison of each reading''s cs_structure.axioms and authority_grounding fields across the three sibling files; identify whether the axioms are logically incompatible (forecloses) or merely differently prioritized (coexists_with).',
    'Determines whether these three readings can coexist as parallel community-relative epistemic frameworks (as authored here, mostly coexists_with/influences) or whether one reading''s adoption structurally forecloses the others within any single institutional framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_structure, conceptual, 'Locating the structural site of disagreement among the three kernel readings.').

omega_variable(
    internal_dissent_suppression_mechanism,
    'Is the suppression experienced by dissenting internal scientists primarily structural (statement-of-faith employment contracts, formal doctrinal review) or internalized (identity fusion with the religious community making exit psychologically costly independent of formal sanction)?',
    'Post-exit trajectory analysis: track whether scientists who formally leave affiliated institutions continue to self-censor or experience distress independent of any remaining formal sanction, versus those whose suppression ends cleanly with employment separation.',
    'If substantially internalized, the effective suppression on this seat is higher than the structural measure captures, and classification should weight the payer seat''s experience more heavily toward snare-like dynamics even without additional formal enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_dissent_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism for dissenting scientists within the movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__creationist_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__creationist_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__creationist_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__creationist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__creationist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(anth_be_t8, anthropological_record__creationist_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(anth_be_t16, anthropological_record__creationist_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(anth_be_t24, anthropological_record__creationist_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(anth_be_t32, anthropological_record__creationist_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(anth_be_t40, anthropological_record__creationist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(anth_su_t8, anthropological_record__creationist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(anth_su_t16, anthropological_record__creationist_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(anth_su_t24, anthropological_record__creationist_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(anth_su_t32, anthropological_record__creationist_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(anth_su_t40, anthropological_record__creationist_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__creationist_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This file is one of three constraints decomposed from the single natural-language label 'the anthropological record reveals human origins.' Each reading (creationist, naturalist, indigenous_epistemology) instantiates a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle. The creationist reading is authored here as tangled_rope (genuine community-cohesion coordination plus enforced conformity and revenue extraction); the naturalist reading is expected to author much lower extraction (open peer-review contestability, no doctrinal exit penalty) closer to rope/mountain depending on which specific empirical claim is at issue; the indigenous_epistemology reading is expected to carry a different extraction profile centered on external credentialist dismissal of oral tradition rather than internal doctrinal enforcement. All three are linked via affects_constraints because institutional resourcing, public curriculum battles, and legal standing in one reading's domain directly shift the legitimacy and resource conditions available to the others (e.g., public school curriculum fights are zero-sum across readings in many jurisdictions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

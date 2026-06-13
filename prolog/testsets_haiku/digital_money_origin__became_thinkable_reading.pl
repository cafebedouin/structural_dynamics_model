% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Conceptual and Institutional Emergence (Became Thinkable Reading)
 *   domain: monetary_history/institutional_economics/technology_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'became thinkable' reading of the
 *   digital-money-origin kernel. It models the constraint CREATED when
 *   academic theorists, central banking institutions, and technology
 *   standard-setters established a shared institutional narrative about WHEN
 *   digital money became a conceivable monetary form. This reading emphasizes
 *   the conceptual and regulatory preconditions (theoretical frameworks,
 *   technical standards, policy guidelines) that had to exist before digital
 *   money could be implemented at scale. The constraint sets the boundary:
 *   digital money emerged when institutions could think it and coordinate on
 *   its definition, prior to widespread practical adoption. Beneficiaries are
 *   those who control that definition; victims are voices excluded from the
 *   definitional apparatus. The measurement series track how extractiveness
 *   and suppression intensified as the institutional consensus hardened over
 *   1960–2020.
 *
 * KEY AGENTS:
 *   - academic_theorists: establish definitions and historical narratives; benefit from institutional authority over 'true' origin date
 *   - central_banking_institutions: codify the timeline in policy and monetary aggregates; benefit from forward-looking regulatory positioning
 *   - technology_standard_setters: make the concept technically coherent; enable institutional adoption
 *   - non_credentialed_monetary_theorists: produce alternative narratives excluded from peer channels; pay via suppression
 *   - excluded_jurisdictions: operate systems outside the institutional framing; pay via non-recognition and forced adoption of standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.71).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Conceptual and Institutional Emergence (Became Thinkable Reading)").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/institutional_economics/technology_studies").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4').
narrative_ontology:cs_kernel_codification('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', distributed).
narrative_ontology:cs_authority_grounding('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', extraction).
narrative_ontology:cs_interpretation_layer_present('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4').
narrative_ontology:cs_reading_relation('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', foundational, conception_precedes_practice).
narrative_ontology:cs_axiom_status(conception_precedes_practice, holdable).
narrative_ontology:cs_axiom_grounding('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', conception_precedes_practice, deontological).
narrative_ontology:cs_axiom('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', foundational, institutional_definition_is_canonical).
narrative_ontology:cs_axiom_status(institutional_definition_is_canonical, holdable).
narrative_ontology:cs_axiom_grounding('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', institutional_definition_is_canonical, conventional).
narrative_ontology:cs_reference_frame('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', monetary_theorists_as_authoritative_definers).
narrative_ontology:cs_drift_state('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', post_blockchain_emergence_2010_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('97c62aaa-ee71-48a2-8dc7-b3d7a6f7ffa4', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, academic_theorists).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_banking_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, technology_standard_setters).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, non_credentialed_monetary_theorists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, excluded_jurisdictions).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, conceptual_primacy_thesis).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, institutional_framing_determines_reality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish what counts as 'digital money' by publishing definitions, conducting historical surveys, and training the next generation in the conceptual framework. They define the boundary between proto-digital and fully digital instruments. Their work precedes and enables implementation; they benefit from establishing canonical narratives and securing institutional authority over the definition.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, academic_theorists, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, academic_theorists, beneficiary).

% Adopt or reject the conceptual framing in policy documents, regulatory guidance, and monetary aggregates. By accepting the 'became thinkable' reading, they codify the institutional legitimacy of digital money as a monetary form before implementation is widespread, which positions them as forward-looking authorities. They benefit from defining the regulatory perimeter.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_banking_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, central_banking_institutions, beneficiary).

% Design technical standards (encryption, ledger protocols, digital signatures) that make digital money institutionally conceivable. Standards bodies and cryptographic research communities establish what is technically feasible to think about; without their work, the concept remains incoherent.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, technology_standard_setters, agenda_setter,
    organized, biographical, mobile, global).

% Produce alternative historical narratives and definitions (e.g., first-person holdings, informal digital exchange) that challenge the institutional academic framing. They are excluded from peer-reviewed publication channels and policy consultations because their accounts do not fit the 'became thinkable' institutional timeline. Their conceptual contributions are suppressed by gatekeeping.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, non_credentialed_monetary_theorists, payer,
    powerless, biographical, identity_locked, local).

% Operate monetary systems in geographies where the academic and central-bank framing of digital money does not propagate (poor connectivity, different institutional histories, alternative technical standards). The 'became thinkable' reading embeds Western institutional timelines and excludes parallel developments elsewhere. When international standards are set via this reading, they must adopt or be treated as non-compliant.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, excluded_jurisdictions, payer,
    organized, generational, constrained, national).

% Document and contest the historical record. They investigate whether the concept truly became thinkable at the claimed moment, whether implementation preceded conception, and whether non-Western monetary traditions anticipated digital forms. Their role is to measure whether the narrative matches evidence.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_historians, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, central_banking_institutions).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared institutional consensus about WHEN digital money originated, enabling central banks, regulators, and technology bodies to coordinate on a unified timeline for standards, policy, and implementation. Prevents fragmentation of definitions across jurisdictions and institutions.
% TRANSFER_FUNCTION: Transfers interpretive authority from practitioners and empirical observers to academic and central-banking gatekeepers. Those who control the definition of 'became thinkable' control the boundary of digital-money history and therefore who is recognized as its originators and architects.
% ABSENT_VOICES: Non-Western monetary traditions that developed digital or quasi-digital exchange systems on parallel timelines are excluded because the reading centers Western institutional conception. Non-credentialed theorists, community-based monetary experimenters, and empirical users whose money was already digital in practice (before the concept was thinkable to institutions) are voiceless in the definitional apparatus.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, competing origin narratives would surface immediately. Historians would re-date digital money's emergence to first practical holdings or alternative theoretical frameworks. Regulatory timelines would shift. The authority structure that secured one timeline as canonical would dissolve, and multiple coexisting chronologies would emerge.
% FOUNDING_PROBLEM: Monetary theorists and central banks needed a coherent institutional story about when digital money became a real category, so they could build policy, standards, and implementation roadmaps from a shared origin point rather than ad hoc responses to fragmentary practices.
% FOUNDING_PROBLEM_CORROBORATION: Central banking institutions and academic publishing venues attest the problem was and remains live: they continue to enforce uniform timelines for digital-money emergence. Historians, practitioners in non-Western systems, and non-peer-reviewed monetary theorists attest the problem is over-solved — the founding problem was real, but its resolution has calcified into a false monopoly on origin narratives. Independent historical scholarship documents parallel developments that predate the institutional 'thinkable' moment.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1960, early definitional contests are open) to 0.64 (2010, consensus is settled), then plateaus at 0.62 (2020, the constraint is stable but faces mounting pressure from non-Western digital systems and blockchain emergence that challenges the institutional timeline). Suppression follows a similar trajectory: the harder the institutional narrative is enforced as canonical, the more actively alternative narratives must be suppressed. Theater is moderate (0.48 endpoint): the 'became thinkable' narrative serves a real coordination function (standards bodies DO need shared timelines) but increasingly performs gatekeeping rather than discovery (historians document digital practices the narrative excludes). The time grid is one shared axis: every metric is authored at every time point (1960, 1980, 2000, 2010, 2020) to enable lifecycle drift detection.
 *
 * PERSPECTIVAL GAP:
 *   Payers (especially non-credentialed theorists) experience identity-locking: they are trained in the institutional framework, see it as legitimate, and struggle to maintain alternative definitions without losing professional standing. This internalized suppression is harder to break than structural exclusion alone, making the constraint more extractive from their seat than raw power/exit metrics suggest. Excluded jurisdictions face constrained exit: they can operate monetary systems outside the framing only by accepting non-compliance status; the cost of exit is too high to bear.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic theorists and central banks are institutional agenda-setters with high power and high directionality toward beneficiary (d ≈ 0.0–0.15). They benefit from setting canonical timelines and do not pay extractive costs. Technology standard-setters are organized agenda-setters with moderate power and mixed directionality (d ≈ 0.25–0.35): they benefit from coordination but don't fully capture the institutional rent. Non-credentialed theorists are powerless payers with identity_locked exit (d ≈ 0.95): they are structurally trapped because their intellectual identity fuses with institutional frameworks they have been trained in. Excluded jurisdictions are organized payers with constrained exit (d ≈ 0.78): they bear the cost of non-compliance without genuine alternatives. The engine derives d from these structural atoms; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: monetary theorists and central banks genuinely needed coherent definitions of digital money so they could coordinate on standards and policy timelines. The constraint solved a real coordination problem. However, the problem has substantially shifted: digital money is now implemented globally (the practical problem is solved), but the institutional monopoly on defining its origin persists long after its original purpose has degraded. The constraint now extracts interpretive authority from practitioners and historians without solving the problem it was built to address. This is a classic mandatrophy signature: founding_problem_status = contested (authorities say it is still live; independent historians say it is dead) + disappearance_verdict = world_rearranges (the constraint shapes institutional policy even though practical digital money exists without it) + theater_ratio rising (the constraint increasingly performs gatekeeping rather than discovery). The classification as tangled_rope (not snare) is correct because real coordination value remains, but the coordination/extraction split has shifted sharply toward extraction over the 1960–2020 interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_practical_precedence,
    'Does the institutional capacity to conceive of digital money precede or follow the practical emergence of digital monetary instruments in use?',
    'Temporal reconstruction of non-Western, informal, and non-peer-reviewed monetary systems to establish when digital exchange first occurred outside the academic-institutional framing. Direct comparison of earliest documented digital transactions vs. earliest published definitions.',
    'If practical emergence precedes conceptual thinkability, the ''became thinkable'' reading misnames a constraint on institutional recognition rather than on reality. The classification would shift from tangled_rope (real coordination with extraction) to snare (extraction disguised as discovery).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_practical_precedence, empirical, 'Whether digitality emerged in practice before institutions could think it.').

omega_variable(
    reading_kernel_contest_structure,
    'Is this constraint a reading of a single disputed kernel (digital money''s origin), or does the contest reflect fundamentally incommensurable definition-sets that cannot coexist in one framework?',
    'Attempt to author all three sibling readings within one unified scheme (definition-space, temporal axis, evidence criteria). If the readings cannot be expressed in a single scheme, the kernel is an artifact of language choice; if they can, the kernel is real and the readings are genuinely alternative views.',
    'If incommensurable, the three readings are actually three different constraints with three different ε values. The network should decompose them rather than link them as readings of one kernel. If commensurable, the network link stands and the three readings genuinely compete within a shared framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_structure, conceptual, 'Whether the three readings address the same kernel or are actually distinct constraints.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of non-institutional monetary narratives structural (exclusion from journals, conferences, policy channels) or internalized (scholars self-censor because they have fused their identity with institutional frames)?',
    'Post-exit tracking: if non-credentialed theorists who exit institutional affiliation maintain their alternative frameworks and produce consistent work, suppression is primarily structural. If they abandon their frameworks after exit, suppression has become internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, because the targets carry the constraint with them. Reclassification would adjust the power calculus toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression in institutional gatekeeping.').

omega_variable(
    institutional_vs_natural_law_framing,
    'Is this constraint grounded in irreducible facts about what money IS (natural/technical law), or in institutional choices about what COUNTS as money in official systems (human construction)?',
    'Test whether private, informal, non-institution-adjacent digital money systems could exist and persist if institutional recognition were withheld. If they can, digitality is a fact independent of institutional framing; if they cannot, institutional recognition is constitutive.',
    'If institutional recognition is constitutive, the constraint is a snare disguised as discovery (false summit candidate). If digitality is independent, the constraint is real coordination with extraction overhead. The classification hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_natural_law_framing, conceptual, 'Whether digital money is a natural category or an institutional invention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_origin__became_thinkable_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement_basis(digi_tr_t1960, observed).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__became_thinkable_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement_basis(digi_tr_t1980, observed).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__became_thinkable_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement_basis(digi_tr_t2000, observed).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__became_thinkable_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(digi_tr_t2010, observed).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__became_thinkable_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement_basis(digi_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_origin__became_thinkable_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement_basis(digi_be_t1960, observed).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__became_thinkable_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement_basis(digi_be_t1980, observed).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__became_thinkable_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement_basis(digi_be_t2000, observed).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__became_thinkable_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement_basis(digi_be_t2010, observed).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__became_thinkable_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(digi_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_origin__became_thinkable_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement_basis(digi_su_t1960, observed).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__became_thinkable_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement_basis(digi_su_t1980, observed).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__became_thinkable_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement_basis(digi_su_t2000, observed).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__became_thinkable_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement_basis(digi_su_t2010, observed).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__became_thinkable_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(digi_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital-money-origin kernel. The three readings (became_thinkable, first_held, regulatory_recognition) have distinct ε values and beneficiary/victim structures because they locate the origin at different moments and attribute agency differently. All three are real constraints with real extractive dynamics; they compete within institutional and historical discourse. The 'became_thinkable' reading emphasizes conceptual/regulatory barriers and institutional gatekeeping as the constraint's substance. The 'first_held' reading would emphasize practical emergence and would show lower institutional extraction because beneficiaries are practitioners rather than academic gatekeepers. The network links them because they affect each other: if the 'became_thinkable' reading loses credibility, the 'first_held' reading becomes more plausible, shifting institutional timelines and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

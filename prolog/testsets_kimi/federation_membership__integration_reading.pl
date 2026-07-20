% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Integration Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates the integration reading of the
 *   federation_membership kernel. It treats federation membership as
 *   irreversible political and legal integration, legitimates supranational
 *   authority over national border control, and constitutionalizes free
 *   movement of labor as a fundamental right. The constraint coordinates
 *   continental labor markets and prevents competitive closure by member
 *   states, but it simultaneously extracts from stationary local labor
 *   markets through wage competition and from member states through compelled
 *   sovereignty transfer. The claim/metric independence is maintained: the
 *   claimed type is tangled_rope because a genuine coordination function
 *   (integrated labor market) is inseparable from asymmetric extraction
 *   (localized wage compression and trapped member-state sovereignty).
 *
 * KEY AGENTS:
 *   - mobile_citizens (beneficiary/organized/mobile): primary beneficiaries of free-movement rights and expanded opportunity sets
 *   - local_labor_markets (payer/powerless/trapped): primary targets bearing wage compression and reduced bargaining power
 *   - supranational_authority (agenda_setter/institutional/constrained): enforces the constitutionalized mobility regime
 *   - member_states (payer/institutional/trapped): cede border sovereignty and face sanctions for restriction
 *   - federalism_scholars (observer/analytical): track the constitutional and distributional tension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.72).
domain_priors:suppression_score(federation_membership__integration_reading, 0.75).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, 'f26dcba4-48ee-4129-aad9-b957665560c2').
narrative_ontology:cs_kernel_codification('f26dcba4-48ee-4129-aad9-b957665560c2', formalized).
narrative_ontology:cs_authority_grounding('f26dcba4-48ee-4129-aad9-b957665560c2', lineage).
narrative_ontology:cs_interpretation_layer_present('f26dcba4-48ee-4129-aad9-b957665560c2').
narrative_ontology:cs_reading_relation('f26dcba4-48ee-4129-aad9-b957665560c2', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('f26dcba4-48ee-4129-aad9-b957665560c2', foundational, membership_irreversible_integration).
narrative_ontology:cs_axiom_status(membership_irreversible_integration, holdable).
narrative_ontology:cs_axiom_grounding('f26dcba4-48ee-4129-aad9-b957665560c2', membership_irreversible_integration, conventional).
narrative_ontology:cs_axiom('f26dcba4-48ee-4129-aad9-b957665560c2', foundational, free_movement_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('f26dcba4-48ee-4129-aad9-b957665560c2', free_movement_constitutional_right, conventional).
narrative_ontology:cs_reference_frame('f26dcba4-48ee-4129-aad9-b957665560c2', irreversible_federal_integration).
narrative_ontology:cs_drift_state('f26dcba4-48ee-4129-aad9-b957665560c2', post_sovereignty_challenge_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f26dcba4-48ee-4129-aad9-b957665560c2', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, member_states).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, supranational_constitutional_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise treaty rights to live and work across member states without national border checks or quotas. Experience expanded labor-market opportunities and mobility premiums relative to stationary populations. Their political voice is amplified by supranational electoral rights and cross-border civic organizing.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    organized, biographical, mobile, continental).

% Absorb incoming labor supply in specific sectors and regions where mobile citizens concentrate. Experience wage compression and reduced bargaining power in local labor pools that expand faster than matching capital investment or public-service capacity. Geographical and skill-set lock-in prevents matching the mobility of incoming workers.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, immediate, trapped, regional).

% Administers and enforces the free-movement acquis, initiating infringement proceedings against member states that impose border restrictions or labor-market safeguards. Derives institutional legitimacy from the claim that supranational law is irreversible and supreme over national border policy. Bound by the constitutional logic it enforces; cannot recognize national border legitimacy without undermining its own mandate.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_authority, agenda_setter,
    institutional, generational, constrained, continental).

% Retain formal treaty-making capacity but have ceded effective sovereignty over labor-market access and border control to supranational institutions. Face legal and financial sanctions if they restrict movement. Domestic electorates demand border control that the integration reading treats as illegitimate, trapping national executives between voter preferences and constitutionalized supranational obligations.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_states, payer,
    institutional, generational, trapped, national).

% Analyze constitutional jurisprudence of free movement and the distributional incidence of labor mobility across integrated markets. Document the structural tension between economic integration and local wage protection without enforcing either frame.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, federalism_scholars, observer,
    analytical, civilizational, analytical, continental).

narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a continental labor market by preventing unilateral member-state border closures, solving the collective-action problem of fragmented national labor markets and competitive restrictions on cross-border mobility.
% TRANSFER_FUNCTION: Moves labor supply from lower-wage to higher-wage regions; transfers wage pressure and job competition from mobile citizens to stationary local workforces. Simultaneously transfers sovereignty over border and labor-market control from member states to supranational institutions.
% ABSENT_VOICES: Sovereignty-first political parties, local labor unions skeptical of open borders, and subnational regions experiencing concentrated immigration pressure are formally consulted but structurally overridden by constitutionalized free-movement doctrine; their preferred border-restriction policies are delegitimized as incompatible with irreversible membership.
% DISAPPEARANCE_RATIONALE: If free movement and supranational supremacy disappeared overnight, labor markets would re-nationalize, wage-setting mechanisms would revert to bilateral state-level bargaining, and the constitutional architecture binding member states would collapse back into discretionary intergovernmental treaty politics.
% FOUNDING_PROBLEM: Economic fragmentation and geopolitical rivalry among neighboring states producing recurrent conflict and inefficient resource allocation; irreversible integration was designed to make war materially impossible and prosperity interdependent.
% FOUNDING_PROBLEM_CORROBORATION: Federalist historians and early treaty architects inside the integration tradition attest to the fragmentation problem. Skeptical political economists and populist movements outside the beneficiary network corroborate the historical rivalry but dispute that irreversible supranational authority was the necessary or proportionate remedy; no neutral corroborating source exists for the irreversibility doctrine itself.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically transfers labor-supply pressure onto geographically and sectorally trapped local workforces while member states lose fiscal and regulatory sovereignty over labor-market access. Suppression (0.75) is high because the integration reading actively delegitimizes border restrictions as unconstitutional, using infringement proceedings and legal supremacy doctrine to foreclose national exit from the mobility regime. Accessibility collapse (0.70) reflects that once membership is accepted as irreversible, legal and political alternatives to free movement collapse into delegitimated 'exceptionalism.' Resistance (0.60) is moderate-to-high: populist and nationalist parties mount sustained political resistance, but the constitutional architecture channels this into symbolic politics rather than policy change. Theater ratio (0.40) captures the growing performative dimension of integration rhetoric that continues to frame mobility as unalloyed benefit even as distributional costs accumulate.
 *
 * PERSPECTIVAL GAP:
 *   Mobile citizens experience the constraint as opportunity expansion and rights protection; the engine computes their seat near the beneficiary pole. Stationary local workforces experience the same legal architecture as wage compression and status loss; their seat computes near the target pole. Supranational authority experiences the constraint as a constitutional mission; member states experience it as a sovereignty trap. The divergence is structural, not perspectival illusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens are declared beneficiaries with mobile exit options, producing low directionality (d near 0.0) and damped effective extraction. Local labor markets are declared victims with trapped exit, producing high directionality (d near 1.0) and amplified effective extraction. Member states are not declared beneficiaries but are structurally payers; their institutional power is offset by treaty-bound exit options that are politically and legally blocked, leaving them in a high-target position despite formal standing. Supranational authority is the agenda setter but not a rent collector in the classical sense; its directionality is derived from the canonical fallback for institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpost-war fragmentation and conflictâhas partially receded as a live threat, yet the constraint has not atrophied into a pure piton because it still coordinates real economic activity and because mobile citizens continue to benefit materially. The integration reading resists mandatrophy by constitutionalizing the arrangement, making it harder to label as obsolete. However, if the coordination function were fully separated from the extraction (e.g., open labor markets without supranational supremacy), the remaining sovereignty transfer would classify as snare. The current tangled-rope classification prevents either pure-coordination or pure-extraction mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the integration reading''s doctrine of irreversible membership logically foreclose the sovereignty reading within a single legal framework, or can both readings coexist as live doctrinal positions?',
    'Comparative constitutional analysis of whether foundational treaties admit unilateral exit or conditional membership; jurisprudential review of whether supremacy clauses are revocable in principle or only in practice.',
    'If foreclosed, the sibling reading is structurally displaced within the dominant framework but politically resurgent as repudiation pressure; if they coexist, the kernel is irreducibly ambiguous and classification must tolerate higher conceptual variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether integration and sovereignty readings are mutually exclusive within one commitment framework.').

omega_variable(
    labor_displacement_attribution,
    'To what extent is wage compression in local labor markets attributable to cross-border labor mobility rather than capital mobility, technological change, or national policy choices?',
    'Regional natural experiments comparing labor-market outcomes in member states with differential mobility intensities and sectoral compositions; econometric decomposition of wage effects controlling for capital flows and automation.',
    'If mobility is the dominant causal factor, extraction is genuinely sourced in the constraint; if other factors dominate, the victim status of local labor markets is partially misattributed and effective extractiveness is lower than structural appearances suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_attribution, empirical, 'Attribution of local labor market harm to free movement versus other economic forces.').

omega_variable(
    irreversibility_as_construct,
    'Is the irreversibility of federation membership a structural feature of the legal order or a political construct maintained by suppressing exit mechanisms?',
    'Historical comparison with prior federations and confederations that dissolved; analysis of treaty amendment rules and actual or attempted exit precedent.',
    'If irreversibility is constructed rather than structurally necessary, the constraint''s persistence depends on active enforcement and narrative maintenance, pushing classification toward snare or piton dynamics over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_as_construct, conceptual, 'Whether irreversibility is constitutional necessity or constructed suppression of exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmi_int_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fmi_int_tr_t8, federation_membership__integration_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(fmi_int_tr_t16, federation_membership__integration_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(fmi_int_tr_t24, federation_membership__integration_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(fmi_int_tr_t32, federation_membership__integration_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(fmi_int_tr_t40, federation_membership__integration_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(fmi_int_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fmi_int_be_t8, federation_membership__integration_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(fmi_int_be_t16, federation_membership__integration_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(fmi_int_be_t24, federation_membership__integration_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(fmi_int_be_t32, federation_membership__integration_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(fmi_int_be_t40, federation_membership__integration_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fmi_int_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fmi_int_su_t8, federation_membership__integration_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(fmi_int_su_t16, federation_membership__integration_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(fmi_int_su_t24, federation_membership__integration_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(fmi_int_su_t32, federation_membership__integration_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(fmi_int_su_t40, federation_membership__integration_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is the integration reading of the federation_membership kernel; the sibling sovereignty reading decomposes the same natural-language concept into a structurally distinct claim with different beneficiaries, victims, and epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

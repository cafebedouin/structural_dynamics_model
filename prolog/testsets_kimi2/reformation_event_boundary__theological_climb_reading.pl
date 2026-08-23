% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Theological Climb Reading of the Reformation Boundary
 *   domain: historical_epistemology/religious_history
 *
 * SUMMARY:
 *   This constraint story models the theological_climb_reading of the
 *   contested kernel reformation_event_boundary. The reading frames the
 *   Reformation (1517-1555) as a genuine doctrinal breakthrough in which
 *   Luther recovered the apostolic teaching of justification by faith alone,
 *   necessitating institutional separation from Rome. From this reading's
 *   framework, the Catholic Church is the victim of necessary theological
 *   correction, Protestant communities are the beneficiaries of restored
 *   doctrine, and reform theologians administer the boundary. The constraint
 *   is analyzed as a tangled rope: it coordinates Protestant identity around
 *   a shared origin narrative while asymmetrically extracting historical
 *   legitimacy and authority from the Catholic magisterium. This is one
 *   reading of three; political_swap and composite_overdetermination readings
 *   are addressed as sibling constraints via network relations and omega
 *   variables.
 *
 * KEY AGENTS:
 *   - reform_theologians: Primary agenda-setter (institutional/generational/constrained) â administers the theological reading and confessional boundaries
 *   - protestant_communities: Primary beneficiary (moderate/generational/identity_locked) â receives doctrinal coordination and identity legitimation
 *   - catholic_magisterium: Primary target (institutional/civilizational/constrained) â bears delegitimization cost and loss of historical authority
 *   - materialist_historians: Excluded observer (organized/generational/analytical) â would argue for socioeconomic drivers but is kept out of confessional historiography
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.62).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.58).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Theological Climb Reading of the Reformation Boundary").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, 'e7ac520d-031b-4656-b89b-d02946755b55').
narrative_ontology:cs_kernel_codification('e7ac520d-031b-4656-b89b-d02946755b55', fixed_text).
narrative_ontology:cs_authority_grounding('e7ac520d-031b-4656-b89b-d02946755b55', lineage).
narrative_ontology:cs_interpretation_layer_present('e7ac520d-031b-4656-b89b-d02946755b55').
narrative_ontology:cs_reading_relation('e7ac520d-031b-4656-b89b-d02946755b55', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('e7ac520d-031b-4656-b89b-d02946755b55', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('e7ac520d-031b-4656-b89b-d02946755b55', foundational, sola_fide_apostolic_recovery).
narrative_ontology:cs_axiom_status(sola_fide_apostolic_recovery, holdable).
narrative_ontology:cs_axiom_grounding('e7ac520d-031b-4656-b89b-d02946755b55', sola_fide_apostolic_recovery, theological).
narrative_ontology:cs_axiom('e7ac520d-031b-4656-b89b-d02946755b55', foundational, institutional_separation_doctrinal_necessity).
narrative_ontology:cs_axiom_status(institutional_separation_doctrinal_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e7ac520d-031b-4656-b89b-d02946755b55', institutional_separation_doctrinal_necessity, theological).
narrative_ontology:cs_reference_frame('e7ac520d-031b-4656-b89b-d02946755b55', scriptural_purity_recovered).
narrative_ontology:cs_drift_state('e7ac520d-031b-4656-b89b-d02946755b55', modern_secular_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e7ac520d-031b-4656-b89b-d02946755b55', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_communities).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reform_theologians).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_magisterium).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the theological reading of the Reformation as a recovery of apostolic doctrine; set confessional boundaries, defend the periodization 1517-1555, and maintain the narrative that institutional separation was doctrinally necessary. Their authority and status depend on the climb narrative remaining credible across Protestant institutions.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reform_theologians, agenda_setter,
    institutional, generational, constrained, continental).

% Receive doctrinal clarity, communal identity, and historical legitimation from the theological reading; their collective self-understanding is coordinated around the narrative of liberation from false doctrine through recovered scriptural purity.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_communities, beneficiary,
    moderate, generational, identity_locked, regional).

% Bears the delegitimizing cost of being framed as doctrinally corrupt and historically superseded; loses symbolic authority, moral legitimacy, and institutional credibility within the reading's framework. Cannot exit the narrative without abandoning its own theological tradition.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_magisterium, payer,
    institutional, civilizational, constrained, universal).

% Would argue for socioeconomic and political drivers of the Reformation but are structurally excluded from confessional historiography and theological curricula; their absence enables the monocausal theological framing to persist unchallenged in ecclesiastical educational contexts.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, materialist_historians, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, protestant_communities).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Protestant communal identity and doctrinal unity around the recovered teaching of justification by faith alone; provides a shared historical origin narrative that binds disparate regional movements into a single Reformation with coherent confessional boundaries.
% TRANSFER_FUNCTION: Moves historical legitimacy, salvific authority, and institutional credibility from the Catholic magisterium to Protestant communities and their theologians.
% ABSENT_VOICES: Materialist and political historians who would argue for socioeconomic primacy; Catholic historians who contest the corruption narrative as the primary cause; secular rulers whose political agency is backgrounded by the theological framing.
% DISAPPEARANCE_RATIONALE: If the theological climb reading vanished, Protestant confessional identity would lose its primary legitimating origin narrative; the tight periodization 1517-1555 would dissolve; Catholic-Protestant ecumenical relations would require renegotiation on non-doctrinal terms; and confessional educational curricula would need fundamental reconstruction.
% FOUNDING_PROBLEM: The alleged corruption of apostolic doctrine by the medieval Catholic Church, particularly the mingling of human works with divine grace in salvation and the practice of indulgences, requiring a return to Scripture as the sole authority.
% FOUNDING_PROBLEM_CORROBORATION: Protestant confessional historians attest the problem from within the benefiting tradition. Modern academic historians and Catholic scholars contest the framing, noting the political economy of indulgences, the diversity of late medieval theology, and the institutional interests served by the corruption narrative. Corroboration from outside the beneficiary set is weak and contested.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial transfer of legitimacy and authority from Catholic to Protestant institutions. Suppression (0.58) captures the active exclusion of Catholic counter-narratives and materialist historiography from confessional frameworks. Theater ratio (0.45) indicates significant performative maintenance of the theological frame, especially during confessionalization, overlaying genuine doctrinal innovation. Accessibility collapse (0.60) measures how thoroughly alternative periodizations and causal explanations are collapsed within Protestant educational and ecclesiastical contexts. Resistance (0.55) registers ongoing Catholic historical contestation and modern academic skepticism toward monocausal theological explanation. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (reform theologians and Protestant communities) experience the constraint as rope-like coordination restoring true doctrine and unifying dispersed movements. The payer seat (Catholic magisterium) experiences the same constraint as extractive delegitimization that historically justifies its supersession. The excluded seat (materialist historians) experiences it as an incomplete frame that suppresses political and economic causation. The engine computes these divergences from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant communities and reform theologians are declared beneficiaries, receiving low directionality near the beneficiary end; they are subsidized by the constraint's operation and experience damped effective extraction. The Catholic magisterium is declared victim, receiving high directionality near the full-target end; its effective extraction is amplified. Materialist historians are excluded rather than directly targeted, so they fall outside the primary beneficiary-victim derivation chain and revert to analytical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by distinguishing genuine theological coordination (the rope function that unifies Protestant doctrine and communal identity) from asymmetric extraction (the transfer of legitimacy and authority from Catholicism). A purely snare reading would miss the real coordination Protestant communities experience around shared doctrine; a purely rope reading would miss the delegitimization cost borne by the Catholic Church and the suppression of alternative historiography. Tangled rope classification captures both functions operating through the same historiographical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the theological_climb_reading of kernel reformation_event_boundary; does the kernel require decomposition into irreducible drivers or is theological primacy the correct structural classification?',
    'Comparative historiographical meta-analysis assessing the explanatory adequacy of monocausal theological framing against multicausal composite and political-swap alternatives.',
    'If the composite reading is correct, this constraint''s extraction from the Catholic Church is partially mitigated by distributed causation and its coordination function is weaker than claimed; if theological primacy holds, the constraint''s type remains tangled_rope with strong coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer uncertainty: position of this reading within the contested kernel').

omega_variable(
    periodization_boundary_artificiality,
    'Does the tight 1517-1555 periodization reflect a genuine theological rupture or a constructed boundary serving confessional identity formation?',
    'Archival analysis of religious practice and belief on both sides of 1517 and 1555; assessment of whether the Peace of Augsburg marks theological resolution or merely political stalemate.',
    'If artificial, accessibility_collapse is lower than measured and the constraint''s enforcement history is more theatrical; if genuine, the coordination function is anchored in real doctrinal innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_boundary_artificiality, empirical, 'Uncertainty about the naturalness of the temporal boundary').

omega_variable(
    apostolic_recovery_or_constructed_narrative,
    'Is the theological reading a genuine recovery of apostolic doctrine (approaching mountain-like coordination) or a constructed narrative serving emergent Protestant institutional formation?',
    'Historical-critical examination of patristic and medieval sources; sociological analysis of institutional identity formation during confessionalization.',
    'If genuine recovery, base extractiveness is lower and the constraint tilts toward rope; if constructed institutional narrative, extractiveness is higher and false-summit detection applies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(apostolic_recovery_or_constructed_narrative, empirical, 'Uncertainty whether the theological kernel is natural or constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__theological_climb_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(refo_tr_t50, reformation_event_boundary__theological_climb_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(refo_tr_t100, reformation_event_boundary__theological_climb_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(refo_tr_t200, reformation_event_boundary__theological_climb_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement(refo_tr_t350, reformation_event_boundary__theological_climb_reading, theater_ratio, 350, 0.43).
narrative_ontology:measurement(refo_tr_t500, reformation_event_boundary__theological_climb_reading, theater_ratio, 500, 0.45).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__theological_climb_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(refo_be_t50, reformation_event_boundary__theological_climb_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(refo_be_t100, reformation_event_boundary__theological_climb_reading, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(refo_be_t200, reformation_event_boundary__theological_climb_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(refo_be_t350, reformation_event_boundary__theological_climb_reading, base_extractiveness, 350, 0.58).
narrative_ontology:measurement(refo_be_t500, reformation_event_boundary__theological_climb_reading, base_extractiveness, 500, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_event_boundary__theological_climb_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(refo_su_t50, reformation_event_boundary__theological_climb_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(refo_su_t100, reformation_event_boundary__theological_climb_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(refo_su_t200, reformation_event_boundary__theological_climb_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement(refo_su_t350, reformation_event_boundary__theological_climb_reading, suppression_requirement, 350, 0.45).
narrative_ontology:measurement(refo_su_t500, reformation_event_boundary__theological_climb_reading, suppression_requirement, 500, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the theological_climb_reading of kernel reformation_event_boundary; it shares the historical event referent with political_swap_reading and composite_overdetermination_reading but differs structurally in epsilon, beneficiary/victim structure, and causal claims. Decomposition follows the epsilon-invariance principle: each reading instantiates a distinct constraint with a stable referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

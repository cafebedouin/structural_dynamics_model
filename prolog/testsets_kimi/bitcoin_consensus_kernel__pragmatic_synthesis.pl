% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__pragmatic_synthesis, []).

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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Pragmatic Synthesis: Base Immutability with Layered Innovation
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The pragmatic synthesis reading of the Bitcoin consensus kernel asserts
 *   that the whitepaper establishes an immutable monetary policy at the base
 *   layer while permitting permissionless innovation on upper layers
 *   (Lightning, sidechains, L2 protocols). This reading attempts to bridge
 *   monetary maximalists, who demand base-layer purity, and utility
 *   advocates, who demand iterative functionality. As a constraint, it
 *   coordinates development and investment by segregating immutable rules
 *   from flexible construction. The cost is borne by ideological coherence:
 *   monetary maximalists experience dilution of their pure-hard-money frame
 *   as upper-layer activity is granted legitimacy within the Bitcoin
 *   ecosystem. The constraint is claimed as a low-extractiveness scaffoldâa
 *   temporary support structure that prevents civil war by spatially
 *   separating conflicting demandsâthough its actual transience is
 *   contested.
 *
 * KEY AGENTS:
 *   - layer_innovators (beneficiary/moderate/mobile)âgain legitimacy to build atop Bitcoin without base-layer political battles
 *   - monetary_maximalists (payer/organized/identity_locked)âbear ideological incoherence as non-base activity is legitimized
 *   - base_layer_operators (agenda_setter/organized/constrained)âenforce base immutability, maintaining the kernel boundary
 *   - pragmatic_investors (beneficiary/powerful/mobile)âbenefit from reduced community conflict and dual-track growth
 *   - utility_rejectionists (excluded/moderate/constrained)âwant base-layer iterability and are not accommodated by the synthesis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.28).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.35).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.28).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Pragmatic Synthesis: Base Immutability with Layered Innovation").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, 'ad99f7c8-30dd-4f39-b6aa-499de82d9b8f').
narrative_ontology:cs_kernel_codification('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', fixed_text).
narrative_ontology:cs_authority_grounding('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', lineage).
narrative_ontology:cs_interpretation_layer_present('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f').
narrative_ontology:cs_reading_relation('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', bitcoin_consensus_kernel__utility_reading, coexists_with).
narrative_ontology:cs_axiom('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', foundational, base_layer_monetary_immutability).
narrative_ontology:cs_axiom_status(base_layer_monetary_immutability, holdable).
narrative_ontology:cs_axiom_grounding('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', base_layer_monetary_immutability, conventional).
narrative_ontology:cs_axiom('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', foundational, upper_layer_innovation_permissible).
narrative_ontology:cs_axiom_status(upper_layer_innovation_permissible, holdable).
narrative_ontology:cs_axiom_grounding('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', upper_layer_innovation_permissible, conventional).
narrative_ontology:cs_reference_frame('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', immutable_base_flexible_layers).
narrative_ontology:cs_drift_state('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', contemporary_layer_contentions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad99f7c8-30dd-4f39-b6aa-499de82d9b8f', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer_innovators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, pragmatic_investors).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, monetary_maximalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build payment channels, sidechains, and L2 protocols atop Bitcoin's base layer. They receive legitimacy from the narrative that their work does not violate the monetary kernel, which reduces political friction and attracts investment. They could theoretically build on other chains but have committed technical and social capital to Bitcoin's ecosystem.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer_innovators, beneficiary,
    moderate, biographical, mobile, global).

% Understand Bitcoin primarily as a hard-money covenant and measure legitimacy by base-layer purity. They experience the legitimization of upper-layer tokens, NFTs, and sidechain activity as a dilution of the original mission, forcing them to either broaden their definition of Bitcoin or accept a community that includes activity they consider parasitic. Their exit is constrained by identity: their social and self-concept is fused with the hardest version of the monetary thesis.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, monetary_maximalists, payer,
    organized, generational, identity_locked, global).

% Operate nodes and mining facilities that validate and enforce consensus rules. Their refusal to run software that alters the monetary policy enforces the immutable base; they do not control what happens on layers but ensure that no layer protocol changes the base state they validate. They can exit to other networks or stop operating, but their hardware and sunk costs constrain them.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_operators, agenda_setter,
    organized, generational, constrained, global).

% Hold Bitcoin as a store of value and allocate capital to layer-2 ventures. They prefer a unified ecosystem where monetary premium and utility growth reinforce each other rather than competing for mindshare. They are mobile across digital assets but have chosen Bitcoin because of its institutional footprint.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, pragmatic_investors, beneficiary,
    powerful, biographical, mobile, global).

% Advocate for iterative improvement of the base protocol itselfâop code upgrades, block size adjustments, or consensus changesâto enable utility directly on the base chain. They are not accommodated by the synthesis, which directs all innovation away from the base layer, and are often excluded from protocol governance conversations that treat base immutability as non-negotiable.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, utility_rejectionists, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the conflict between sound-money maximalists and utility-seeking developers by spatially segregating immutable monetary rules (base layer) from permissionless innovation (upper layers), allowing both camps to operate without direct collision.
% TRANSFER_FUNCTION: Moves legitimacy, developer attention, and capital from base-layer political contestation to upper-layer experimentation; moves ideological purity and coherence from monetary maximalists to a hybridized community frame.
% ABSENT_VOICES: Absolute utility advocates who demand base-layer protocol iteration and hard maximalists who reject any legitimacy for non-base activity are structurally underrepresented in the synthesis; their absence creates the appearance of consensus.
% DISAPPEARANCE_RATIONALE: If the pragmatic synthesis vanished, layer innovators might face maximalist delegitimization campaigns, maximalists might push for explicit base-layer purity covenants, and the ecosystem could polarize into warring camps; however, some argue the base layer would simply persist unchanged while market forces sort the layers organically.
% FOUNDING_PROBLEM: How to preserve a credibly neutral, immutable monetary policy kernel while permitting the system to scale and acquire useful functionality without recurring contentious hard forks.
% FOUNDING_PROBLEM_CORROBORATION: Layer-2 developers and open-source researchers outside the maximalist camp attest the problem remains live, citing ongoing scaling needs. Monetary maximalists contest the framing, asserting the only founding problem was sound money and that the synthesis introduces false requirements. No fully neutral corroborator exists; the problem statement is itself contested.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, contested).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).
:- end_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint does not materially extract wealth from its victims; the primary cost is ideological (loss of maximalist coherence). Suppression is moderate (0.35) because the synthesis must actively marginalize absolutist claims that reject upper-layer legitimacy. Theater is moderate-low (0.25): the 'layers are not Bitcoin' versus 'layers are Bitcoin' debate involves performative boundary-policing, but substantial technical work occurs. Accessibility collapse is moderate (0.40): once one accepts the synthesis, alternative pure readings become less viable socially, but they remain technically accessible. Resistance is moderate (0.45): maximalists actively resist the legitimization of layers through social media, fork advocacy, and protocol rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   From the layer innovator seat, the constraint is rope-like coordination that prevents base-layer interference. From the monetary maximalist seat, it is a snare-like dilution of the founding covenant. The engine computes this divergence from identical structural data: the difference is directionalâthe same arrangement subsidizes one seat and extracts from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Layer innovators and pragmatic investors are structural beneficiaries (low d): the constraint subsidizes their activity by legitimizing it and shielding it from base-layer politics. Monetary maximalists are structural targets (high d): the constraint extracts ideological coherence from them by reframing what counts as legitimate Bitcoin activity. Base layer operators sit near symmetric (d ~0.5): they enforce the boundary but do not themselves capture the gains or bear the ideological cost. The derivation holds without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this arrangement as pure extraction (it genuinely coordinates conflicting camps) or as pure coordination (it imposes real costs on maximalist identity and coherence). If the founding problemâhow to scale and innovate without breaking monetary policyâwere dead, and the arrangement persisted purely to legitimize layer rent-seeking, it would drift toward tangled_rope or snare. The measurements show modest extraction accumulation but low theater, consistent with a live scaffold still serving its transition function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    base_upper_boundary_ambiguity,
    'Where exactly does the base layer end and the upper layer begin, and does activity that anchors data to the base chain (e.g., inscriptions) constitute kernel violation?',
    'Technical taxonomy of layer-2 versus base-layer transactions, community consensus surveys, and case studies of contentious base-layer usage.',
    'If the boundary is inherently unstable, the scaffold cannot permanently segregate the two camps and will collapse into either maximalist or utility readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(base_upper_boundary_ambiguity, conceptual, 'Ambiguity in the base/upper boundary definition.').

omega_variable(
    scaffold_transience,
    'Is the pragmatic synthesis a transitional compromise awaiting technological or political resolution, or a permanent equilibrium?',
    'Historical trajectory analysis: if layer-2 ecosystems achieve functional autonomy or base-layer ossification becomes irreversible, the scaffold''s sunset may be realized; if cycles of contention recur, the scaffold is likely permanent.',
    'If transient, classification as scaffold is validated; if permanent, reclassification as rope or tangled_rope may be warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_transience, preference, 'Whether the pragmatic synthesis is temporary or permanent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bck_ps_tr_t0, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bck_ps_tr_t2, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2, 0.12).
narrative_ontology:measurement(bck_ps_tr_t4, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 4, 0.15).
narrative_ontology:measurement(bck_ps_tr_t6, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 6, 0.18).
narrative_ontology:measurement(bck_ps_tr_t8, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 8, 0.22).
narrative_ontology:measurement(bck_ps_tr_t10, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(bck_ps_be_t0, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bck_ps_be_t2, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2, 0.18).
narrative_ontology:measurement(bck_ps_be_t4, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(bck_ps_be_t6, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 6, 0.23).
narrative_ontology:measurement(bck_ps_be_t8, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(bck_ps_be_t10, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 10, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(bck_ps_su_t0, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bck_ps_su_t2, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2, 0.22).
narrative_ontology:measurement(bck_ps_su_t4, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(bck_ps_su_t6, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(bck_ps_su_t8, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(bck_ps_su_t10, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, utility_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel decomposes into three readingsâmaximalist, pragmatic synthesis, and utilityâbecause the whitepaper's sparse textual kernel underdetermines whether immutability is total, layered, or minimal. Each reading claims the same fixed text but produces a structurally distinct constraint with different beneficiary/victim profiles and extractiveness levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

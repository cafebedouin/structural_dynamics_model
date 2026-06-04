% ============================================================================
% CONSTRAINT STORY: ministerial_responsibility__agency_accountability_gap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ministerial_responsibility__agency_accountability_gap_reading, []).

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
 *   constraint_id: ministerial_responsibility__agency_accountability_gap_reading
 *   human_readable: Ministerial Responsibility: Agency Accountability Gap (Next Steps Reading)
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   The Next Steps agencies reading of ministerial responsibility doctrine
 *   holds that operational delegation to chief executives broke the chain of
 *   accountability: ministers answer for policy only, not for operational
 *   execution. This reading instantiates one structural reading of the
 *   contested kernel 'ministerial responsibility.' The constraint emerges
 *   when Parliament's oversight capacity is suppressed by the delegated
 *   architecture: the House cannot remove chief executives, cannot compel
 *   testimony about operational decisions, cannot enforce consequences for
 *   failure beneath policy level. Simultaneously, the constraint coordinates
 *   genuine governmental function: the minister can focus on policy strategy
 *   while operations are delegated to dedicated managers. This hybrid
 *   (coordination + extraction) is the defining feature of the tangled rope
 *   classification. The constraint's extractiveness has risen over the
 *   measurement interval (0.22 → 0.48) as the original Next Steps framework
 *   (1988+) matured. Theater ratio has also risen (0.35 → 0.58) as
 *   ministerial claims of accountability have become increasingly
 *   performative — ministers state 'the buck stops here' while the
 *   institutional architecture prevents the buck from stopping. Suppression
 *   requirement has increased (0.48 → 0.65) as select committees and other
 *   alternative accountability structures have emerged: maintaining the
 *   delegation framework now requires active suppression of alternative
 *   oversight mechanisms, not merely their absence.
 *
 * KEY AGENTS:
 *   - Ministers (Executive Branch): Primary beneficiary (institutional/arbitrage) — benefit from operational shield; can claim policy authority while deflecting operational failure
 *   - Chief Executives (Next Steps Agencies): Primary target (powerful/constrained) — coordinate operational delivery but lack parliamentary accountability; risk being scapegoated
 *   - Parliament and Select Committees: Secondary victim (organized/mobile) — oversight capacity is suppressed; cannot access chief executives; constrained from enforcement
 *   - Convention of Ministerial Responsibility: Institutional mechanism (institutional/arbitrage) — persists as performative theater; original content is hollowed
 *   - Public Accountability: Victim collective (powerless/trapped) — cannot access the chain of command; accountability is routed through minister who claims distance from operations
 *   - Analytical Observer: Sees natural law risk (analytical/analytical) — risks naturalizing delegation architecture as inevitable rather than chosen
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ministerial_responsibility__agency_accountability_gap_reading, 0.48).
domain_priors:suppression_score(ministerial_responsibility__agency_accountability_gap_reading, 0.65).
domain_priors:theater_ratio(ministerial_responsibility__agency_accountability_gap_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ministerial_responsibility__agency_accountability_gap_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ministerial_responsibility__agency_accountability_gap_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ministerial_responsibility__agency_accountability_gap_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ministerial_responsibility__agency_accountability_gap_reading, tangled_rope).
narrative_ontology:human_readable(ministerial_responsibility__agency_accountability_gap_reading, "Ministerial Responsibility: Agency Accountability Gap (Next Steps Reading)").
narrative_ontology:topic_domain(ministerial_responsibility__agency_accountability_gap_reading, "legal/doctrinal").

domain_priors:requires_active_enforcement(ministerial_responsibility__agency_accountability_gap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ministerial_responsibility__agency_accountability_gap_reading, '99c9fb35-c54d-4d15-a071-ac2fda5112e9').
narrative_ontology:cs_kernel_codification('99c9fb35-c54d-4d15-a071-ac2fda5112e9', fixed_text).
narrative_ontology:cs_authority_grounding('99c9fb35-c54d-4d15-a071-ac2fda5112e9', lineage).
narrative_ontology:cs_interpretation_layer_present('99c9fb35-c54d-4d15-a071-ac2fda5112e9').
narrative_ontology:cs_reading_relation('99c9fb35-c54d-4d15-a071-ac2fda5112e9', ministerial_responsibility__resignation_norm_decay_reading, coexists_with).
narrative_ontology:cs_reading_relation('99c9fb35-c54d-4d15-a071-ac2fda5112e9', ministerial_responsibility__select_committee_accountability_reading, influences).
narrative_ontology:cs_axiom('99c9fb35-c54d-4d15-a071-ac2fda5112e9', foundational, operational_delegation_severs_accountability).
narrative_ontology:cs_axiom_status(operational_delegation_severs_accountability, holdable).
narrative_ontology:cs_axiom_grounding('99c9fb35-c54d-4d15-a071-ac2fda5112e9', operational_delegation_severs_accountability, instrumental).
narrative_ontology:cs_axiom('99c9fb35-c54d-4d15-a071-ac2fda5112e9', secondary, single_neck_principle).
narrative_ontology:cs_axiom_status(single_neck_principle, holdable).
narrative_ontology:cs_axiom_grounding('99c9fb35-c54d-4d15-a071-ac2fda5112e9', single_neck_principle, deontological).
narrative_ontology:cs_reference_frame('99c9fb35-c54d-4d15-a071-ac2fda5112e9', crichel_down_standard).
narrative_ontology:cs_drift_state('99c9fb35-c54d-4d15-a071-ac2fda5112e9', post_next_steps_agency_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99c9fb35-c54d-4d15-a071-ac2fda5112e9', '').
narrative_ontology:cs_kernel_id(ministerial_responsibility__agency_accountability_gap_reading, ministerial_responsibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ministerial_responsibility__agency_accountability_gap_reading, ministers_executive_branch).
narrative_ontology:constraint_victim(ministerial_responsibility__agency_accountability_gap_reading, convention_clarity).
narrative_ontology:constraint_victim(ministerial_responsibility__agency_accountability_gap_reading, parliamentary_oversight).
narrative_ontology:constraint_victim(ministerial_responsibility__agency_accountability_gap_reading, public_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC AND PARLIAMENTARY OVERSIGHT (SNARE) — Parliament's power to hold accountable is suppressed by the operational delegation. Ministers answer only for policy choices made at cabinet level; chief executives answer to ministers privately. The House cannot remove officials; it cannot compel testimony about operational decisions; it cannot enforce consequences for failure beneath policy level. The accountability mechanism is broken without replacement. Trapped in inability to access the chain of command.
constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINISTER AS BENEFICIARY (ROPE) — The minister experiences this as pure coordination: the delegation of operational detail to the chief executive enables the minister to focus on policy direction and strategic choices. The minister has exit options (can move departments, can serve in cabinet without operational headaches, can claim distance from implementation failures). The minister benefits from the operational shield — responsibility is divided, and the minister answers for policy intent, not operational execution. The extraction is limited because the minister genuinely obtains benefit from the division of labor.
constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CHIEF EXECUTIVE (TANGLED ROPE) — The chief executive coordinates operational delivery (genuine coordination function); the constraint enables the department to function. However, the chief executive is also extracted: answerable to the minister privately, not answerable to Parliament, constrained from public explanation, carrying reputational risk for failures while the minister can deflect to policy intent. The chief executive has constrained exit — professional expectation is to serve the department, and public accountability deficit means the chief executive cannot appeal to Parliament directly if scapegoated. Mixed coordination and extraction.
constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SELECT COMMITTEES (ASPIRATIONAL TANGLED ROPE) — Select committees (with elected chairs, cross-party membership) have emerged as an alternative accountability structure. They can call witnesses beyond ministers, can sustained inquiry into operations, can publish findings. They coordinate accountability in a way the chamber cannot. However, they face extraction: no power to remove officials, findings are often ignored, ministers can decline sensitive testimony by invoking cabinet confidentiality. The select committee sees the constraint as both coordination mechanism (committees now do inquiry the chamber doesn't) and extraction mechanism (their inquiries lack enforcement teeth). Mobile exit because committees can choose to intensify scrutiny or step back; extraction is real but not total.
constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CONVENTION ITSELF (PITON) — The convention of ministerial responsibility persists in language (ministers claim accountability for their departments) but operates as theater with low functional content. The original convention bound ministers to fall if their departments failed. The Next Steps agencies reading inverts this: operational delegation severs that bond. Ministers claim responsibility ('the buck stops here') while the institutional architecture prevents the buck from doing anything. The convention is maintained through ritual (government statements about accountability) while its mechanism is neutered. Theater ratio is high because the performance of accountability persists even as its structure is hollowed.
constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW READING (FALSE SUMMIT) — From a civilizational analytical view, administrative delegation is an inevitable feature of modern governance: complex operations cannot be directed by elected officials. Some gap between policy responsibility and operational execution is inherent to the scale of contemporary government. This perspective risks naturalizing what is actually a chosen institutional architecture. The Next Steps reforms (1988+) deliberately created the gap; it is not a law of nature but a policy decision. The analytical observer's mountain classification will trigger false summit detection because identifiable beneficiaries (ministers) exist and the victim set is clear.
constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ministerial_responsibility__agency_accountability_gap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ministerial_responsibility__agency_accountability_gap_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ministerial_responsibility__agency_accountability_gap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ministerial_responsibility__agency_accountability_gap_reading, TR),
    TR >= 0.70.

:- end_tests(ministerial_responsibility__agency_accountability_gap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The reading identifies asymmetric benefit: ministers are shielded from operational consequences while retaining policy authority; chief executives bear operational risk while lacking parliamentary exit. The extraction is not total (snare) because genuine coordination occurs — operational delegation does enable specialized management. The measurement trajectory (0.22 → 0.48 over 15 years) reflects the maturation of the Next Steps framework and the ossification of the accountability gap. As the system has settled, the extractive character has become more apparent. Suppression (0.65): High. Accountability is actively suppressed: Parliament cannot remove officials (structural gate), the operational-policy boundary is enforced to exclude scrutiny (performative gate), and alternative mechanisms (select committees) are constrained in enforcement power. The trajectory (0.48 → 0.65) shows that maintaining the gap requires increasing suppression effort — early Next Steps had lower suppression because the boundary was still being negotiated; mature Next Steps requires active defense of the boundary. Theater ratio (0.58): Moderate-high. Ministerial claims of accountability ('the buck stops here') are performative when the institutional architecture prevents the buck from reaching ministers. Select committee inquiries are theater insofar as they lack enforcement power. The trajectory (0.35 → 0.58) reflects the divergence between rhetoric and mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces sharp perspectival divergence. The minister sees rope (coordination of operational delegation). Parliament sees snare (suppressed oversight). The chief executive sees tangled rope (mixed coordination and extraction). Select committees see tangled rope transitioning toward scaffold (temporary suppression as alternative accountability structures mature). The convention itself has become piton (performative maintenance of hollowed content). The analytical observer risks seeing mountain (inevitable delegation as law of administrative scale) but the false summit signature fires because identifiable beneficiaries exist. The core gap is between the beneficiary's experience (clean division enabling efficient government) and the victim's experience (suppressed accountability with no structural remedy).
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction runs from Parliament/public toward ministers. Ministers have arbitrage options (move departments, serve outside operational agencies, claim policy distance from implementation) and institutional power. Parliament has trapped or constrained options (cannot exit the constitutional obligation to oversee government). Chief executives have constrained options (professional expectation to serve, no direct parliamentary appeal). The suppression mechanism is two-fold: structural (Parliament cannot access operational detail) and performative (the ministerial responsibility rhetoric obscures that the mechanism no longer functions). Directionality values (d) reflect this asymmetry: ministers have low d (beneficiaries with exit); Parliament has high d (victim with limited exit); chief executives have moderate-high d (victims with some constrained exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading resolves mandatrophy by identifying the constraint as tangled rope (not pure extraction, because genuine coordination occurs) while acknowledging that the coordination function has become a cover for extraction (ministers claim operational responsibility while architecture prevents enforcement). The piton perspective (the convention as performative theater) shows the degradation pathway: the convention began as functional rule (Crichel Down standard: resign on departmental failure) and has become theatrical claim (ministers state accountability while institutional architecture prevents its enforcement). The tangled rope classification captures this simultaneously: genuine operational coordination (the delegation enables the department to function) + extractive asymmetry (ministers escape consequences while chief executives cannot). The false summit signature at the analytical level reveals the risk of naturalizing delegation as inevitable law of administrative scale — it is actually a chosen policy that distributes accountability asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_policy_boundary_definition,
    'Where is the boundary between ''operational detail'' (delegated to chief executives) and ''policy choice'' (retained by ministers) actually drawn in practice?',
    'Case study analysis of parliamentary inquiries, select committee findings, and resignation/non-resignation episodes; identify disputes over boundary location and who controls the definition',
    'If boundary is stable and respected: extraction is limited to the mechanism itself (transparency suppression). If boundary is contested and fluid: ministers exploit it opportunistically to deflect accountability, raising extractiveness. If boundary collapses entirely: constraint reverts to rope or snare depending on outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_policy_boundary_definition, empirical, 'Operational-policy boundary definition and enforcement').

omega_variable(
    alternative_accountability_structures,
    'Do select committees and other non-ministerial accountability mechanisms constitute genuine accountability or substitute theater that obscures the accountability gap?',
    'Longitudinal analysis: do select committee findings lead to policy change? Are officials ever removed or demoted based on committee inquiries? Does public compliance increase post-inquiry? Compare outcomes for committees with no minister witnesses vs. committees with full cooperation.',
    'If genuine: accountability has migrated (not suppressed) and the constraint reconfigures to rope or scaffold with sunset clause as select committee power matures. If theater: accountability remains suppressed and extraction remains high; select committees are performative alternatives to substantive change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_accountability_structures, empirical, 'Whether select committees provide substantive accountability or substitute theater').

omega_variable(
    constitutional_authority_grid,
    'Does the reading commit to parliamentary sovereignty or ministerial executive authority as the grounding principle, and does that choice foreclose the resignation norm reading?',
    'Doctrinal analysis of how the two readings justify their positions; identify whether one reading''s foundational principle (sovereignty vs. executive efficiency) logically entails rejection of the other reading''s foundational principle in a single constitutional framework.',
    'If readings are logically incompatible: the kernel contains a genuine foreclosure relationship (rare). If readings coexist as different parties'' positions: kernel contains influence relationship, not foreclosure. This determines the committer structure of the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_authority_grid, conceptual, 'Logical compatibility of this reading''s constitutional grounding with the resignation norm reading').

omega_variable(
    suppression_mechanism_structure,
    'Is suppression of accountability rooted in the legal/doctrinal architecture of delegation itself, or in the political culture that tolerates non-resignation?',
    'Test by removing the delegation architecture (restore direct minister responsibility for operations) while holding political culture constant. If accountability improves: suppression is architectural. If accountability remains suppressed: suppression is cultural. Conversely, strengthen accountability norms (formal resignation expectations) while preserving delegation: if accountability improves, suppression is cultural.',
    'If suppression is architectural: the extractiveness score reflects the design itself; reform requires structural change (re-integration or new accountability mechanisms). If suppression is cultural: extractiveness can be reduced by norm change without architectural reform; the Next Steps framework could function with stronger accountability culture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structure, empirical, 'Whether suppression is architectural (delegation design) or cultural (resignation norm decay)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ministerial_responsibility__agency_accountability_gap_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minres_gap_tr_t0, ministerial_responsibility__agency_accountability_gap_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(minres_gap_tr_t5, ministerial_responsibility__agency_accountability_gap_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(minres_gap_tr_t10, ministerial_responsibility__agency_accountability_gap_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(minres_gap_tr_t15, ministerial_responsibility__agency_accountability_gap_reading, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(minres_gap_be_t0, ministerial_responsibility__agency_accountability_gap_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(minres_gap_be_t5, ministerial_responsibility__agency_accountability_gap_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(minres_gap_be_t10, ministerial_responsibility__agency_accountability_gap_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(minres_gap_be_t15, ministerial_responsibility__agency_accountability_gap_reading, base_extractiveness, 15, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(minres_gap_su_t0, ministerial_responsibility__agency_accountability_gap_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(minres_gap_su_t5, ministerial_responsibility__agency_accountability_gap_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(minres_gap_su_t10, ministerial_responsibility__agency_accountability_gap_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(minres_gap_su_t15, ministerial_responsibility__agency_accountability_gap_reading, suppression_requirement, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ministerial_responsibility__agency_accountability_gap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ministerial_responsibility__agency_accountability_gap_reading, ministerial_responsibility__resignation_norm_decay_reading).
narrative_ontology:affects_constraint(ministerial_responsibility__agency_accountability_gap_reading, ministerial_responsibility__select_committee_accountability_reading).

% DUAL FORMULATION NOTE:
% This reading of ministerial responsibility is one of three constraints in the kernel family. All three share the same kernel (the convention of ministerial responsibility) but disagree on what has happened to it: this reading says the architecture broke the chain (operational delegation severed accountability); the resignation norm reading says the norm itself decayed (ministers stopped resigning); the select committee reading says accountability relocated (inquiry moved off the floor). The network links all three as sibling readings. Each has its own epsilon, beneficiary/victim structure, and type classification, but they are mutually relevant interpretations of the same constitutional commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

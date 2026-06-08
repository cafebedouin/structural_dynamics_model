% ============================================================================
% CONSTRAINT STORY: survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_survival_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: survival_competence_reading
 *   human_readable: Ritual as Survival Competence Encoding (Catastrophe Memory Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the
 *   catastrophe_memory_kernel: the reading that frames ritual practice as a
 *   mechanism for encoding and transmitting survival competence for
 *   communities facing existential threat (persecution, assimilation,
 *   cultural elimination). The kernel itself — the stabilized commitment to
 *   preserving catastrophe memory through ritual — is interpreted by multiple
 *   communities in structurally distinct ways: as survival training (this
 *   reading), as symbolic continuity, as trauma encoding/processing, or as
 *   boundary maintenance. This story analyzes only the survival-competence
 *   reading. From this perspective, ritual reenactment of historical
 *   persecution and response patterns serves as a practical technology for
 *   rehearsing decision-making under existential threat, preserving adaptive
 *   behaviors that might be lost during periods of safety, and maintaining
 *   collective knowledge of catastrophe-response patterns across generations.
 *   The constraint exhibits Tangled Rope structure: genuine coordination
 *   function (preserving survival competence that has proven adaptive)
 *   coupled with asymmetric extraction (mandatory participation, identity
 *   lock for members, behavioral constraint, opportunity cost). The
 *   measurement trajectory shows declining extractiveness over time as
 *   external threat pressure has diminished (from 0.55 to 0.35) and as
 *   explicit competence-transmission systems have developed, yet
 *   theater_ratio has risen slightly (from 0.22 to 0.35), indicating that as
 *   the threat becomes historical rather than immediate, the ritual's
 *   symbolic/performative content has grown relative to its functional
 *   content. The suppression_requirement has also declined, reflecting that
 *   enforcement mechanisms have softened as assimilation pressure has (in
 *   many diaspora contexts) diminished — younger cohorts participate with
 *   less coercive pressure than historical cohorts.
 *
 * KEY AGENTS:
 *   - Community membership (powerless/identity_locked): Bears extraction through mandatory participation and identity fusion; experiences the ritual as simultaneously protective and constraining
 *   - Community leadership (organized/constrained): Stewards ritual transmission; experiences it as coordination mechanism solving knowledge-preservation problem; benefits from social cohesion and authority it generates
 *   - Youth with high assimilation pressure (powerless/trapped): Faces maximal extraction at biographical time horizon; ritual demands conflict with assimilation pathways; cannot exit without relational rupture
 *   - Diaspora reformers (organized/mobile): Advocate for voluntary participation and modernized competence transmission; possess agency and alternative pathways; see ritual as temporary structure transiting toward explicit systems
 *   - Academic observer (institutional/arbitrage): Views ritual from outside; sees it as degraded or vestigial performance; theater_ratio indicates symbolic maintenance of tradition rather than functional rehearsal
 *   - Catastrophe threat (threat agent, not human actor): The persistent risk of persecution/assimilation that ritual preparation addresses; threat level has declined over the measurement interval but remains non-zero
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(survival_competence_reading, 0.38).
domain_priors:suppression_score(survival_competence_reading, 0.42).
domain_priors:theater_ratio(survival_competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(survival_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(survival_competence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(survival_competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(survival_competence_reading, "Ritual as Survival Competence Encoding (Catastrophe Memory Reading)").
narrative_ontology:topic_domain(survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(survival_competence_reading, '7c8a6aae-1d1a-4b4f-80b7-a87234959738').
narrative_ontology:cs_kernel_codification('7c8a6aae-1d1a-4b4f-80b7-a87234959738', fixed_text).
narrative_ontology:cs_authority_grounding('7c8a6aae-1d1a-4b4f-80b7-a87234959738', lineage).
narrative_ontology:cs_interpretation_layer_present('7c8a6aae-1d1a-4b4f-80b7-a87234959738').
narrative_ontology:cs_reading_relation('7c8a6aae-1d1a-4b4f-80b7-a87234959738', survival_competence_reading__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c8a6aae-1d1a-4b4f-80b7-a87234959738', survival_competence_reading__trauma_encoding_reading, influences).
narrative_ontology:cs_reading_relation('7c8a6aae-1d1a-4b4f-80b7-a87234959738', survival_competence_reading__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('7c8a6aae-1d1a-4b4f-80b7-a87234959738', foundational, ritual_encodes_practical_competence).
narrative_ontology:cs_axiom_status(ritual_encodes_practical_competence, holdable).
narrative_ontology:cs_axiom_grounding('7c8a6aae-1d1a-4b4f-80b7-a87234959738', ritual_encodes_practical_competence, empirically_contingent).
narrative_ontology:cs_axiom('7c8a6aae-1d1a-4b4f-80b7-a87234959738', foundational, rehearsal_preserves_adaptive_patterns).
narrative_ontology:cs_axiom_status(rehearsal_preserves_adaptive_patterns, holdable).
narrative_ontology:cs_axiom_grounding('7c8a6aae-1d1a-4b4f-80b7-a87234959738', rehearsal_preserves_adaptive_patterns, instrumental).
narrative_ontology:cs_reference_frame('7c8a6aae-1d1a-4b4f-80b7-a87234959738', catastrophe_response_readiness).
narrative_ontology:cs_drift_state('7c8a6aae-1d1a-4b4f-80b7-a87234959738', contemporary_diaspora_contexts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c8a6aae-1d1a-4b4f-80b7-a87234959738', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(survival_competence_reading, community_resilience_capacity).
narrative_ontology:constraint_beneficiary(survival_competence_reading, intergenerational_knowledge_transmission).
narrative_ontology:constraint_victim(survival_competence_reading, assimilation_pressure).
narrative_ontology:constraint_victim(survival_competence_reading, individual_autonomy_within_ritual_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITY MEMBER (TANGLED ROPE) — Participates in ritual rehearsal of catastrophe-response patterns; identity fused with survival practices passed down through generations. Structurally mobile (could physically leave) but identity-locked — exiting the ritual would require abandoning the community identity constituted through shared catastrophe memory and survival competence. Experiences genuine coordination function (ritual preserves adaptive capacity) alongside extraction (mandatory participation, behavioral constraint, temporal burden). The binding mechanism is cognitive/identity rather than material, yet functional capacity is preserved.
constraint_indexing:constraint_classification(survival_competence_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY LEADERSHIP (ROPE) — Organizes and maintains ritual transmission; experiences the constraint primarily as coordination problem solved. Leadership benefits from ritual's function as knowledge-preservation mechanism and from the social cohesion it produces. Exit costs are high (abandoning leadership role, losing legitimacy) but not insurmountable. Sees ritual as functional tool serving genuine community need. Extraction experienced is moderate and justified by coordination benefit.
constraint_indexing:constraint_classification(survival_competence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: YOUTH WITH HIGH ASSIMILATION PRESSURE (SNARE) — Faces maximal extraction at biographical time horizon. Subject to ritual demands while also facing external assimilation pressure that makes ritual participation costly (peer rejection, cultural alienation, temporal burden competing with assimilation pathways). Trapped exit options — cannot leave community without severe relational rupture; cannot avoid ritual without social ostracism. Extraction is highest here because the ritual's function (survival competence for hypothetical catastrophe) is abstract and distant while the costs (temporal, social, identity conflict) are concrete and immediate.
constraint_indexing:constraint_classification(survival_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: ACADEMIC OBSERVER (PITON) — Views ritual from outside as symbolic/theatrical practice. From civilizational distance, the survival-competence encoding appears degraded or vestigial — the catastrophe memories are historical (not current threat) and the ritual function seems performative rather than practical. Sees the ritual continuing through institutional inertia and identity maintenance rather than because the survival mechanism is still active. Theater ratio indicates most of the ritual's activity is now symbolic maintenance of tradition rather than functional rehearsal of response patterns.
constraint_indexing:constraint_classification(survival_competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIASPORA REFORMERS (SCAFFOLD) — Organized group seeking to preserve survival competence encoding while reducing identity-lock severity. Advocates for ritual modernization: reinterpreting catastrophe memory as historical education rather than identity prerequisite; teaching survival competence through explicit skill transmission (emergency preparedness, community resilience) rather than ritual reenactment; making participation optional rather than mandatory. Sees the constraint as temporary — traditional ritual will naturally sunset as explicit competence systems develop and assimilation reduces categorical threat. High agency and clear exit path make this a scaffold classification.
constraint_indexing:constraint_classification(survival_competence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From deepest analytical view, ritual encoding of adaptive capacity appears as immutable natural law: any group facing existential threat must develop and transmit survival competence; ritual is a naturally emergent technology for encoding and rehearsing catastrophe-response patterns in pre-literate and low-literacy contexts. This perspective risks naturalizing what is actually a contingent institutional choice. The constraint presents as 'inherent to collective memory' when it is actually 'institutionalized extraction justified by reference to survival function.'
constraint_indexing:constraint_classification(survival_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(survival_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(survival_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(survival_competence_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(survival_competence_reading, TR),
    TR >= 0.70.

:- end_tests(survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine coordination function — ritual does preserve and transmit adaptive survival patterns that have demonstrated value in historical catastrophe contexts. However, extraction is present in the form of mandatory participation, identity-lock mechanisms that make exit costly, and opportunity costs (time spent in ritual that could be allocated elsewhere). The moderate level reflects that extraction is not maximized (participation rates remain high voluntarily; community identity value is real) but is also not zero (behavioral constraint, assimilation cost asymmetry). The declining trend (0.55 → 0.35) reflects decreased external threat pressure and the development of alternative competence-transmission systems. Suppression (0.42): Moderate. Barriers to exiting the ritual include identity-fusion costs, community ostracism risk, and loss of categorical safety-net the ritual provides. However, suppression is not severe — many individuals do exit or modify participation without complete community rupture; formal legal barriers do not prevent participation variation. Theater ratio (0.35): Moderate. In early period (0.22), ritual was primarily functional — response patterns were practiced because catastrophe threat was acute and survival competence was immediately pragmatic. As threat declined, symbolic content increased; ritual continues but increasingly for identity preservation and historical continuity rather than active emergency preparedness. Current level (0.35) suggests approximately 1/3 of ritual activity is now performative/symbolic, while 2/3 retains functional content.
 *
 * PERSPECTIVAL GAP:
 *   The survival-competence reading produces radically different classifications depending on the observer's structural position and time horizon. Community members with identity-lock see Tangled Rope — real coordination function combined with inescapable identity requirement. Youth facing assimilation pressure see Snare — the ritual's protective function is abstract and delayed while extraction is concrete and immediate. Leadership sees Rope — genuine problem solved, extraction justified and modest. Diaspora reformers see Scaffold — a temporary structure whose function can be preserved through modernization. Academic observers see Piton — a degraded ritual maintained through inertia whose actual survival function is historical. The civilizational analytical view risks Mountain — naturalizing the constraint as inherent to collective memory when it is actually a contingent institutional choice about how to encode and transmit adaptive patterns. The perspectival gaps here track both power differences (powerless vs. organized) and time-horizon differences (biographical vs. generational vs. civilizational): the constraint looks entirely different at different time scales.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from: (1) agent power relative to the constraint, (2) exit options available to the agent, and (3) whether the agent is a beneficiary or victim of the constraint's operation. Community members who are powerless and identity-locked have high d (approaching 1.0) — they are targets of the constraint's extraction mechanism, unable to exit without severe costs. Yet d is not maximum (not 1.0) because beneficiary status partially derives from the actual survival-competence preservation that benefits them: they benefit from living in a community with preserved adaptive capacity, even though they bear costs of mandatory participation. Community leadership with organized power and constrained exit has low d (approaching 0.0) — they are beneficiaries collecting social authority and cohesion benefits from ritual maintenance, with exit costs high but not insurmountable. Youth with high assimilation pressure have maximum d (near 1.0) — they are targets bearing extraction costs while the constraint's beneficiary function (survival competence for hypothetical catastrophe) feels distant and irrelevant at biographical time scale. Academic observers have zero or negative d — they experience no extraction, only analytical distance. The engine computes effective extraction (χ) from d and other factors; the perspectives that show high χ are those with high d, those that are trapped/identity-locked, and those at biographical time horizons where the constraint's abstract benefits are least salient.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is the phenomenon where a constraint's original mandate has outlived its functional necessity but the constraint persists through institutional inertia and theatrical maintenance. This constraint shows early-stage mandatrophy symptoms: the founding mandate was catastrophe-response preparation (survival training); the original threat context (acute persecution) has diminished in most diaspora contexts; yet ritual practice persists and is increasingly justified through symbolic/identity frames rather than through explicit survival-competence arguments. The measurement trajectory supports this analysis: extractiveness declining (threat pressure reduced, functional necessity diminished) but theater ratio rising (symbolic content increasing relative to practical content). The constraint resolves mandatrophy by identifying that multiple readings of the same kernel are available and functionally viable. The ritual CAN persist under the symbol_continuity reading even if the survival_competence reading's mandate has expired. Diaspora reformers represent organized resistance to mandatrophy — their scaffold perspective seeks to preserve competence transmission while acknowledging that mandatory ritual is no longer the optimal form. If the constraint evolves toward voluntary participation and explicit competence systems, the ritual will have transited from Tangled Rope (mandatory coordination-extraction hybrid) to either Rope (voluntary coordination for identity/symbol preservation) or degraded Piton (ritual continuing as theatrical performance). The mandatrophy is not resolved by the constraint vanishing but by the mandate shifting from 'survival training for active threat' to 'identity and symbol preservation for historical memory.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_catastrophe_threat_status,
    'Is the catastrophe that the ritual encodes still an active threat, or is ritual persistence based on historical memory without current existential necessity?',
    'Historical comparison: correlation between ritual intensity/participation rates and actual threat level (persecution, assimilation pressure, material catastrophe risk); comparison to periods when threat was demonstrably higher vs. lower',
    'If threat is still active: ritual is functional survival mechanism (Rope/Tangled Rope from community perspective). If threat is historical: ritual is performance maintaining identity memory but not functional survival training (Piton/Snare depending on enforcement). Classification changes from genuine coordination problem to theatrical inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_catastrophe_threat_status, empirical, 'Whether the encoded catastrophe risk is still an active threat or historical memory').

omega_variable(
    competence_transfer_efficacy,
    'Does ritual participation actually transmit survival competence (behavioral patterns, decision-making frameworks, resource management) more effectively than explicit instruction?',
    'Comparative outcome analysis: competence measures for youth raised with ritual vs. without; measurement of specific survival behaviors under stress (actual emergency response, community coordination); tracking of communities that transitioned from ritual-based to explicit-based transmission and efficacy changes',
    'If ritual transmission is superior: classification tilts toward Rope (genuine functional coordination). If equivalence or inferiority: classification tilts toward Snare/Piton (ritual persists as identity marker, not functional mechanism). Changes mandate justification from survival necessity to identity preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_transfer_efficacy, empirical, 'Efficacy of ritual-based vs. explicit-instruction competence transmission').

omega_variable(
    assimilation_as_victim_vs_threat,
    'Is ''assimilation pressure'' a victim of the ritual constraint (something the ritual damages) or an external threat that the ritual defends against?',
    'Causal-direction analysis: does ritual practice strengthen assimilation resistance (in which case assimilation pressure is the threat, not the victim), or does mandatory ritual participation create assimilation damage by making youth targets for dominant-culture rejection (in which case ritual harms the very thing it claims to preserve)?',
    'If assimilation is the threat: beneficiary logic is vindicated (ritual protects against it). If ritual causes assimilation damage: the constraint is extraction masked as protection. Shifts from Tangled Rope toward Snare. Changes who experiences extraction — moves from ''youth resisting assimilation'' to ''youth sacrificed to preserve elder identity.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilation_as_victim_vs_threat, empirical, 'Whether assimilation pressure is a threat the ritual defends against or a harm the ritual causes').

omega_variable(
    kernel_reading_identity,
    'This constraint is ONE reading of the catastrophe_memory_kernel — the reading that frames ritual as survival-competence encoding. Is this framing, or is it actually the symbol_continuity_reading masquerading as competence transmission?',
    'Interview-based comparison: ask community members whether they understand ritual as (a) practical survival training, (b) symbolic continuity of tradition, (c) trauma encoding/processing, or (d) boundary maintenance. Measure which framing is dominant within the community vs. which framing generates greatest adaptive benefit. Examine historical documentation of ritual origin — was it explicitly designed as survival training or as continuity mechanism?',
    'If community frame is symbol_continuity: this constraint''s ε is misattributed and this reading is a misclassification. Reclassify to symbol_continuity_reading with different ε and beneficiary structure. If survival training is actually secondary rationale: extraction mechanism is clearer (ritual persists for identity reasons, justified retroactively through survival narrative). If trauma encoding is primary: competence transmission is secondary and ε should reflect therapeutic function, not military preparedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether ritual is understood and functions as survival training or as symbolic/trauma/boundary function').

omega_variable(
    voluntary_participation_feasibility,
    'Can survival competence encoding be preserved if ritual participation becomes voluntary rather than mandatory?',
    'Pilot comparison: communities that maintain ritual function with voluntary participation vs. mandatory participation; measurement of competence transmission rates, identity preservation rates, and community resilience in both configurations; long-term tracking of voluntary-participation communities',
    'If voluntary participation sustains function: enforcement mechanism is not necessary for competence preservation, and classification shifts from Tangled Rope toward Rope (coordination without extraction). Scaffold perspective is validated. If competence collapses without mandatory participation: enforcement is functionally necessary, classification remains Tangled Rope but beneficiary logic is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_feasibility, empirical, 'Whether mandatory enforcement is necessary for survival competence encoding function').

omega_variable(
    catastrophe_memory_kernel_contest,
    'This reading frames the catastrophe_memory_kernel as about COMPETENCE. The sibling readings frame it as about SYMBOL, TRAUMA, and BOUNDARY respectively. Can a single community hold multiple readings simultaneously, or are they genuinely foreclosed alternatives?',
    'Ethnographic analysis: within single community, identify which parties hold which reading; measure whether they produce contradictory interpretations of ritual action or compatible ones; trace historical moments when one reading has overtaken another — was there explicit ideological displacement, or gradual reframing?',
    'If readings are genuinely coexistent: multiple interpretations can be authoritatively held and the engine''s inferred_coupling_protocol should show cross-reading influence rather than foreclosure. If readings are sequentially displaced: later reading represents institutional capture or adaptive reinterpretation; affects understanding of mandate obsolescence (mandatrophy). If readings foreclose each other: may indicate that kernel itself is under-specified and different communities have actually institutionalized different kernels under one label.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_memory_kernel_contest, conceptual, 'Whether sibling readings of the catastrophe_memory_kernel coexist or foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(survival_competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_comp_tr_t0, survival_competence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(surv_comp_tr_t10, survival_competence_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(surv_comp_tr_t20, survival_competence_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(surv_comp_tr_t30, survival_competence_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(surv_comp_be_t0, survival_competence_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(surv_comp_be_t10, survival_competence_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(surv_comp_be_t20, survival_competence_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(surv_comp_be_t30, survival_competence_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(surv_comp_su_t0, survival_competence_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(surv_comp_su_t10, survival_competence_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(surv_comp_su_t20, survival_competence_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(surv_comp_su_t30, survival_competence_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(survival_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(survival_competence_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(survival_competence_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(survival_competence_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_kernel. The sibling constraints (symbol_continuity_reading, trauma_encoding_reading, boundary_maintenance_reading) analyze the same ritual practice but from different functional interpretations. Each has its own epsilon value reflecting what function that reading emphasizes. This reading's epsilon (0.38) reflects survival-competence as primary function; symbol_continuity might have lower epsilon if symbolic preservation requires less extraction; trauma_encoding might have different suppression profile if processing function is primary. The four readings are distinct constraints linked by common phenomenology (the ritual) but with different structural analyses. See catastrophe_memory_kernel documentation for the kernel's contested nature and the methodological choice to decompose into reading-specific constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

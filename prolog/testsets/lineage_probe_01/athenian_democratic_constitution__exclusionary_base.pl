% ============================================================================
% CONSTRAINT STORY: athenian_democratic_constitution__exclusionary_base
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_athenian_democratic_constitution__exclusionary_base, []).

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
 *   constraint_id: athenian_democratic_constitution__exclusionary_base
 *   human_readable: Athenian Democratic Constitution — Exclusionary Base Reading
 *   domain: political/historical
 *
 * SUMMARY:
 *   Athens' democratic constitution is one of history's most instructive
 *   cases of how a coordination mechanism (participatory deliberation among
 *   equals) can rest structurally on extraction (slavery, gender exclusion,
 *   metic disability, tributary imperialism). The exclusionary-base reading
 *   isolates the constraint that makes Athenian democracy possible: the
 *   decision to fund the leisure required for political participation by
 *   externalizing labor onto populations legally excluded from the demos.
 *   This is neither accidental nor peripheral to the system. The structure is
 *   explicit in Aristotle: slaves and women are 'natural slaves' and
 *   'naturally' subject; metics are perennial aliens; tributary allies are
 *   subordinate. The constraint is a snare from the perspective of the
 *   excluded — they cannot exit, alternatives are suppressed, and extraction
 *   is total. It is a rope from the perspective of the included — they
 *   experience it as pure coordination. The democratic ideology performs as a
 *   piton — it describes the system as rule by the people, rule by the many,
 *   equality under law — without acknowledging that 'the people' means one
 *   resident in five and that their equality is financed by the unfreedom of
 *   the rest. The constraint's extractiveness has risen slightly over the
 *   interval (0.65 → 0.70) as imperial tribute accumulated and the citizen
 *   body expanded (demographically), increasing the labor burden on slaves
 *   and the strategic importance of tributary subordination. The suppression
 *   requirement has also risen (0.78 → 0.85) as resistance to exclusion
 *   became more evident — the Melian dialogue, slave revolts, and metic
 *   petitions required increasingly explicit legal and military enforcement.
 *   This reading contests the sibling interpretations:
 *   accountability_machinery claims the system is self-correcting (false if
 *   the constraint's exclusion is invisible to the accountability
 *   mechanisms), assembly_supremacy claims the people rule (begging the
 *   question of who counts as 'the people'), and sortition_and_rotation
 *   claims the lottery is democratic (within the bounded demos only). All
 *   three sibling readings can coexist with this one within different
 *   political communities — they compete for narrative authority over what
 *   'Athenian democracy' means, but no single reading forecloses another in
 *   the logic of democracy itself. Rather, this reading influences the others
 *   by establishing that any reading must account for the exclusionary
 *   foundation or risk inheriting the system's blind spot.
 *
 * KEY AGENTS:
 *   - Adult Male Citizens: Primary beneficiary (institutional/arbitrage) — capture political equality, leisure for participation, and prestige; benefit from extraction without experiencing coercion
 *   - Enslaved Populations: Primary victim (powerless/trapped) — unfree labor supplies the surplus that funds citizen leisure; no legal exit, no political voice, no right to self-determination
 *   - Women: Primary victim (powerless/trapped) — excluded from citizenship, deliberation, and public life; legal minors throughout life under male guardianship; work (domestic and productive) enables male participation
 *   - Metics (Resident Aliens): Secondary victim (moderate/constrained) — excluded from political participation and land ownership despite residence and economic contribution; resource barriers and legal disability prevent exit to full participation
 *   - Tributary Allied States: Secondary victim (moderate/constrained) — integrated into Athenian political and military systems; extraction of tribute and military service; theoretical exit path foreclosed by military power asymmetry
 *   - Democratic Ideology: Institutional actor (institutional/arbitrage) — maintains legitimacy through rhetoric of equality and rule by the many; benefits from naturalization of exclusion; performs piton function (theatrical maintenance of authority despite degraded function)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(athenian_democratic_constitution__exclusionary_base, 0.68).
domain_priors:suppression_score(athenian_democratic_constitution__exclusionary_base, 0.82).
domain_priors:theater_ratio(athenian_democratic_constitution__exclusionary_base, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(athenian_democratic_constitution__exclusionary_base, extractiveness, 0.68).
narrative_ontology:constraint_metric(athenian_democratic_constitution__exclusionary_base, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(athenian_democratic_constitution__exclusionary_base, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(athenian_democratic_constitution__exclusionary_base, snare).
narrative_ontology:human_readable(athenian_democratic_constitution__exclusionary_base, "Athenian Democratic Constitution — Exclusionary Base Reading").
narrative_ontology:topic_domain(athenian_democratic_constitution__exclusionary_base, "political/historical").

domain_priors:requires_active_enforcement(athenian_democratic_constitution__exclusionary_base).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(athenian_democratic_constitution__exclusionary_base, '2faf8d8f-8223-4c9b-a760-fc9a19350f39').
narrative_ontology:cs_kernel_codification('2faf8d8f-8223-4c9b-a760-fc9a19350f39', fixed_text).
narrative_ontology:cs_authority_grounding('2faf8d8f-8223-4c9b-a760-fc9a19350f39', lineage).
narrative_ontology:cs_interpretation_layer_present('2faf8d8f-8223-4c9b-a760-fc9a19350f39').
narrative_ontology:cs_reading_relation('2faf8d8f-8223-4c9b-a760-fc9a19350f39', athenian_democratic_constitution__accountability_machinery, influences).
narrative_ontology:cs_reading_relation('2faf8d8f-8223-4c9b-a760-fc9a19350f39', athenian_democratic_constitution__assembly_supremacy, influences).
narrative_ontology:cs_reading_relation('2faf8d8f-8223-4c9b-a760-fc9a19350f39', athenian_democratic_constitution__sortition_and_rotation, influences).
narrative_ontology:cs_axiom('2faf8d8f-8223-4c9b-a760-fc9a19350f39', foundational, citizenship_is_exclusive_by_nature).
narrative_ontology:cs_axiom_status(citizenship_is_exclusive_by_nature, holdable).
narrative_ontology:cs_axiom_grounding('2faf8d8f-8223-4c9b-a760-fc9a19350f39', citizenship_is_exclusive_by_nature, conventional).
narrative_ontology:cs_axiom('2faf8d8f-8223-4c9b-a760-fc9a19350f39', foundational, leisure_for_deliberation_requires_external_labor_supply).
narrative_ontology:cs_axiom_status(leisure_for_deliberation_requires_external_labor_supply, overridden).
narrative_ontology:cs_axiom_grounding('2faf8d8f-8223-4c9b-a760-fc9a19350f39', leisure_for_deliberation_requires_external_labor_supply, empirically_contingent).
narrative_ontology:cs_reference_frame('2faf8d8f-8223-4c9b-a760-fc9a19350f39', athenian_citizenship_by_birth_and_gender).
narrative_ontology:cs_drift_state('2faf8d8f-8223-4c9b-a760-fc9a19350f39', fifth_century_bce_imperial_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2faf8d8f-8223-4c9b-a760-fc9a19350f39', '').
narrative_ontology:cs_kernel_id(athenian_democratic_constitution__exclusionary_base, athenian_democratic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(athenian_democratic_constitution__exclusionary_base, adult_male_citizens).
narrative_ontology:constraint_victim(athenian_democratic_constitution__exclusionary_base, women).
narrative_ontology:constraint_victim(athenian_democratic_constitution__exclusionary_base, slaves).
narrative_ontology:constraint_victim(athenian_democratic_constitution__exclusionary_base, metics).
narrative_ontology:constraint_victim(athenian_democratic_constitution__exclusionary_base, tributary_allies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED AND METIC POPULATIONS (SNARE) — No formal exit from slavery; metics cannot become citizens or own land; women are permanent legal minors under male guardianship. Suppression is structural and total. Extraction flows directly: labor, tribute, and the leisure time that enables male citizen participation in the assembly are funded by their unfreedom. Maximum experienced extractiveness from this position.
constraint_indexing:constraint_classification(athenian_democratic_constitution__exclusionary_base, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRIBUTARY ALLIED STATES (TANGLED ROPE) — Forced membership in the Delian League with substantial coordination benefits (military protection, trade access, security infrastructure) alongside asymmetric extraction (tribute, naval service, deference to Athens' courts). Exit is theoretically possible but materially constrained by military power asymmetry. Genuine coordination function (collective defense) layered with extraction. Suppression of alternatives through military threat.
constraint_indexing:constraint_classification(athenian_democratic_constitution__exclusionary_base, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADULT MALE CITIZEN BODY (ROPE) — Beneficiaries of the exclusionary system. Experiences the constraint as pure coordination: equal political participation, shared deliberation, leisure for public life. The constraint solves their collective action problem (how to share power without hierarchy) by externalizing costs onto the excluded. From this position, the system appears as a successful coordination mechanism — no coercion felt, only the coordination benefit. Net beneficiary; arbitrage exit available (emigration, though rare and costly).
constraint_indexing:constraint_classification(athenian_democratic_constitution__exclusionary_base, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: DEMOCRATIC IDEOLOGY AS INSTITUTIONAL ACTOR (PITON) — The rhetoric of Athenian democracy — equality, freedom, rule by the people — persists as institutional theater masking the structural exclusion. The ideology is performative: it describes the system to the included without acknowledging that 'the people' means specifically free adult males. The piton classification reflects that the ideology's function (legitimation) has degraded into pure mythology once extended beyond its original scope. Theater ratio is lower (0.45) in this reading because the material coordination function is real — the system genuinely does enable equal deliberation among the included — but the ideological presentation is substantially false.
constraint_indexing:constraint_classification(athenian_democratic_constitution__exclusionary_base, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a detached civilizational view, one might frame the Athenian exclusion as a natural constraint: 'any large-scale deliberative system must bound participation for practical reasons; exclusion is inevitable.' This naturalizes what the exclusionary-base reading reveals as contingent: the decision to fund leisure through slavery, the specific boundaries of the citizen body, the legal structures enforcing gender exclusion. The mountain classification is a false summit — the engine will flag the presence of identifiable beneficiaries and structural enforcement mechanisms as contradicting the natural law framing. This perspective instantiates the oracle gap: the analytical observer needs the framework to see what their universalizing position prevents them from acknowledging.
constraint_indexing:constraint_classification(athenian_democratic_constitution__exclusionary_base, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(athenian_democratic_constitution__exclusionary_base_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(athenian_democratic_constitution__exclusionary_base, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(athenian_democratic_constitution__exclusionary_base, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(athenian_democratic_constitution__exclusionary_base, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(athenian_democratic_constitution__exclusionary_base, TR),
    TR >= 0.70.

:- end_tests(athenian_democratic_constitution__exclusionary_base_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The adult male citizens extract direct benefit from the system — leisure time for assembly participation, political equality within the demos, prestige and honor in the imperial system — and this extraction is substantial. However, it is not the purest snare because the system does provide genuine coordination benefits to the included (shared deliberation, collective security, rule-of-law protections). The 0.68 value reflects the snare classification: extraction is primary, but a coordination function exists (within the bounded demos). The value is lower than pure extraction (0.72+) because the system is not merely extractive for the beneficiaries — it also solves their coordination problem. Suppression (0.82): Very high. The exclusion is maintained through explicit legal structures (citizenship law restricting birth/gender/status), property law (women cannot own, metics cannot own land), military power (enforcing tributary subordination), and social norms (gender segregation, slavery legitimization). Alternatives are suppressed: women cannot petition for political rights; slaves cannot legally organize; tributary allies cannot exit without military consequence; metics cannot become citizens. The suppression is structural and enforced. Theater ratio (0.45): Moderate-low. This reading reveals the democratic rhetoric as substantially performative — 'rule by the people' and 'citizen equality' describe only the included, and the ideology obscures the structural exclusion. However, the ratio is not as high as a pure piton (0.70+) because the system does genuinely enable equal deliberation among the included; the coordination function is real, not entirely performative. The theater is in the ideological presentation (naturalization of exclusion, universal claims about democracy), not in the deliberative mechanics themselves. The rising trajectory (0.38 → 0.45) reflects increasing ideological elaboration as the system's scope expanded and required more explicit justification.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces the maximal perspectival gap in the Athenian democracy kernel. The included (adult male citizens) experience rope: coordination, equality, legitimacy. The excluded (slaves, women, metics, tributary allies) experience snare: extraction, suppression, no exit. The democratic ideology experiences piton: its function (legitimation) persists despite being based on a false premise (that the system is democratic in any universal sense). The analytical observer risks experiencing mountain (naturalizing the exclusion as inherent to large-scale deliberation) until the exclusionary-base reading reveals the false summit. The core perspectival divergence is about what counts as 'the constraint': the included see it as the coordination mechanism (assembly, isonomia, equal deliberation); the excluded see it as the exclusion itself (enslavement, legal disability, military subordination). These are not the same constraint — they have different ε values, different beneficiary/victim structures, and different classification outcomes. The kernel contest is resolved by recognizing that all readings are true within their own perspectival scope, but this reading uniquely requires accounting for the structural foundation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's relationship to the extraction flow. The adult male citizens are net beneficiaries: they experience arbitrage-level exit options (emigration is possible though rare; they can exit the system by leaving Athens). Their d value is low (~0.15-0.25), reflecting beneficiary status + arbitrage exit → negative χ (they experience the constraint as enabling rather than constraining). The enslaved populations have d = 0.95+ (full target, trapped exit): they bear the full extraction cost with no exit option. Metics have d = 0.70-0.80 (high target, constrained exit): substantial extraction with theoretical but materially blocked exit. Tributary allies have d = 0.65-0.75 (target, constrained exit due to military asymmetry): significant extraction with exit blocked by power imbalance. The Democratic Ideology itself has d = 0.20-0.30 (institutional beneficiary): it benefits from the naturalization of exclusion and can arbitrage into alternative legitimacy framings if questioned. The engine derives these d values from the beneficiary/victim declarations and exit options, producing the perspectival gap where beneficiaries see rope and victims see snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that the Athenian system contains BOTH a genuine coordination function (participatory deliberation among the included) AND asymmetric extraction (labor expropriation, legal disability, imperial subordination). The classification is unambiguously snare from the perspective of the excluded and rope from the perspective of the included. The mandatrophy is not 'is this coordination or extraction?' but 'for whom does the constraint function as each?' The system does genuine coordination work for the beneficiaries (solves their collective action problem); it inflicts pure extraction on the victims (no coordination benefit, only labor and suppression). The indexical tuple resolves this: (powerless, biographical, trapped, regional) produces snare; (institutional, immediate, arbitrage, regional) produces rope. No contradiction — the system is simultaneously both, observed from different positions. The exclusionary-base reading contributes to mandatrophy resolution by making the structural foundation visible: the reason the beneficiaries experience coordination and the victims experience extraction is that the system is engineered precisely to produce that effect. If the engineering were hidden (if the democratic ideology successfully naturalized exclusion), mandatrophy would remain unresolved. This reading makes the engineering visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_of_exclusion_boundary,
    'Was the specific boundary of exclusion (adult male citizens vs women, slaves, metics) a practical necessity for participatory democracy or a contingent political choice?',
    'Comparative analysis of alternative democratic systems (other Greek poleis, pre-democratic governance); counterfactual: what if metics or freed slaves had been included? Would deliberative dysfunction result or only redistribution of power?',
    'If necessary: mountain classification becomes more defensible. If contingent: exclusion appears as engineered extraction, snare classification confirmed. Sibling readings (accountability_machinery, assembly_supremacy, sortition_and_rotation) all depend on this axiom remaining uncontested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_exclusion_boundary, conceptual, 'Whether the exclusion boundary was necessity or choice').

omega_variable(
    tribute_extraction_vs_collective_security,
    'What proportion of tributary wealth flowed to collective defense (infrastructure, navy, fortifications) versus elite enrichment (public building programs, artistic patronage, imperial prestige)?',
    'Archaeological and historical accounting of tribute allocation; comparison of defensive capacity with tributary burden; analysis of who benefited from monumental construction (citizen labor, contracts, prestige)',
    'If primarily collective defense: tangled_rope classification more defensible for tributary allies (genuine coordination function). If primarily enrichment: snare classification confirmed (pure extraction). Directly affects whether the sibling reading (assembly_supremacy) can claim to represent the demos justly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribute_extraction_vs_collective_security, empirical, 'Proportion of tribute used for defense vs enrichment').

omega_variable(
    counterfactual_citizen_leisure_without_slavery,
    'Would participatory democracy (frequent assembly, jury service, magistracy) been structurally possible if the male citizen population had to supply their own subsistence through productive labor rather than living on slave-generated surplus?',
    'Historical analysis of leisure time required for assembly participation (travel, deliberation, votes); comparison with agricultural labor requirements; modeling of per-capita productivity needed to support assembly participation without slavery',
    'If possible: exclusion appears as contingent choice rather than natural constraint; snare classification strengthened. If impossible: exclusion is revealed as structural requirement of the system''s own logic; mountain classification partly vindicated. This is the deepest axiom contest between exclusionary_base and the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_citizen_leisure_without_slavery, conceptual, 'Whether leisure democracy was structurally possible without slavery').

omega_variable(
    reading_contest_axiom_overriding,
    'If it could be demonstrated that the exclusion was contingent (not necessary), would the Democratic Ideology piton perspective collapse into snare, or would the ideology shift its legitimacy grounding to something else?',
    'Historical-philosophical: what would modern Athenians (or democratic theorists invoking Athens) do with the knowledge that exclusion was chosen, not necessary? Would the ideology acknowledge the foundation and reground on ''we chose to exclude'' (forecloses assembly_supremacy as neutral), or would it persist in denial (piton degradation continues)?',
    'Routes to axiom_overriding drift state for the Democratic Ideology institution. If overridden: the ideology''s authority degrades (piton + acknowledged drift). If persisted in: the ideology''s extraction function becomes explicit (snare classification of the ideology itself). Affects whether the sibling reading (accountability_machinery) can claim the system is self-correcting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_axiom_overriding, preference, 'Whether axiom-overriding would degrade or transform the democratic ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(athenian_democratic_constitution__exclusionary_base, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adc_excl_tr_t0, athenian_democratic_constitution__exclusionary_base, theater_ratio, 0, 0.38).
narrative_ontology:measurement(adc_excl_tr_t20, athenian_democratic_constitution__exclusionary_base, theater_ratio, 20, 0.42).
narrative_ontology:measurement(adc_excl_tr_t40, athenian_democratic_constitution__exclusionary_base, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(adc_excl_be_t0, athenian_democratic_constitution__exclusionary_base, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(adc_excl_be_t20, athenian_democratic_constitution__exclusionary_base, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(adc_excl_be_t40, athenian_democratic_constitution__exclusionary_base, base_extractiveness, 40, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(adc_excl_su_t0, athenian_democratic_constitution__exclusionary_base, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(adc_excl_su_t20, athenian_democratic_constitution__exclusionary_base, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(adc_excl_su_t40, athenian_democratic_constitution__exclusionary_base, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(athenian_democratic_constitution__exclusionary_base, resource_allocation).
narrative_ontology:affects_constraint(athenian_democratic_constitution__exclusionary_base, athenian_democratic_constitution__accountability_machinery).
narrative_ontology:affects_constraint(athenian_democratic_constitution__exclusionary_base, athenian_democratic_constitution__assembly_supremacy).
narrative_ontology:affects_constraint(athenian_democratic_constitution__exclusionary_base, athenian_democratic_constitution__sortition_and_rotation).

% DUAL FORMULATION NOTE:
% The athenian_democratic_constitution kernel has four distinct readings, each modeling different structural claims about what the 'constitution' (politeia) actually is. The exclusionary-base reading focuses on the exclusion that funds participation. The accountability_machinery reading focuses on the exit mechanisms. The assembly_supremacy reading focuses on the deliberative institution. The sortition_and_rotation reading focuses on the randomization principle. These are not alternative measurements of one constraint but four separate constraints with different ε values, each instantiating a different claim about the kernel. The exclusionary_base reading (ε=0.68, snare) is downstream of and shapes the others: if exclusion is the foundation, then accountability mechanisms operate only within the bounded demos, assembly supremacy applies only to the included, and sortition applies only to citizens. The other readings can coexist with this one within the same historical system but represent different epistemic commitments about what makes Athens 'democratic.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

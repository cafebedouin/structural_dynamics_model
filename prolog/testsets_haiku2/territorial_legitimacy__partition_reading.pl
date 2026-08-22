% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via UN Partition and State Recognition
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   UN Resolution 181 (1947) and subsequent international recognition
 *   established a partition framework legitimating both Israeli and
 *   Palestinian statehood within defined borders. The partition reading
 *   treats international legal recognition and fixed boundaries as the source
 *   of legitimate sovereign authority. However, the constraint operates
 *   asymmetrically: Israel's borders are defended and enforced through
 *   military and legal apparatus; Palestine's borders remain contested and
 *   substantially unfulfilled. The displacement of Palestinians in 1948 and
 *   after is the structural extraction the partition generates — the cost of
 *   creating two states is borne by those expelled from their homes. Israeli
 *   settlements beyond the 1948 partition line (post-1967 occupation) operate
 *   in the legal gray zone: they benefit from security and access but pay the
 *   cost of illegitimacy under the partition frame itself. This reading is
 *   one of three contested frameworks for territorial legitimacy in this
 *   kernel; the other readings (security-necessity and indigenous-continuity)
 *   offer alternative accounts of who has legitimate claim to the land and on
 *   what basis.
 *
 * KEY AGENTS:
 *   - Internationally recognized Israel: agenda-setter and beneficiary, enforces partition boundaries selectively
 *   - Internationally recognized Palestine: nominal beneficiary, limited statehood apparatus, identity-locked to the partition frame
 *   - Displaced Palestinians: powerless payers, bear the extraction cost of partition (displacement, refugee status, no return)
 *   - Israeli settlers beyond 1967 lines: powerful but operating in legal gray zone, pay through delegitimacy under the partition frame
 *   - UN and international community: agenda-setter, establishes and selectively enforces partition framework
 *   - Regional security actors: excluded, advocate alternative legitimacy frames
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.67).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.71).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via UN Partition and State Recognition").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '136f3e53-fd2f-41f4-bfdf-244802178c25').
narrative_ontology:cs_kernel_codification('136f3e53-fd2f-41f4-bfdf-244802178c25', fixed_text).
narrative_ontology:cs_authority_grounding('136f3e53-fd2f-41f4-bfdf-244802178c25', lineage).
narrative_ontology:cs_interpretation_layer_present('136f3e53-fd2f-41f4-bfdf-244802178c25').
narrative_ontology:cs_reading_relation('136f3e53-fd2f-41f4-bfdf-244802178c25', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_reading_relation('136f3e53-fd2f-41f4-bfdf-244802178c25', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('136f3e53-fd2f-41f4-bfdf-244802178c25', foundational, partition_boundary_fixes_legitimacy).
narrative_ontology:cs_axiom_status(partition_boundary_fixes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('136f3e53-fd2f-41f4-bfdf-244802178c25', partition_boundary_fixes_legitimacy, conventional).
narrative_ontology:cs_axiom('136f3e53-fd2f-41f4-bfdf-244802178c25', foundational, international_recognition_constitutes_statehood).
narrative_ontology:cs_axiom_status(international_recognition_constitutes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('136f3e53-fd2f-41f4-bfdf-244802178c25', international_recognition_constitutes_statehood, conventional).
narrative_ontology:cs_reference_frame('136f3e53-fd2f-41f4-bfdf-244802178c25', un_partition_framework_1948).
narrative_ontology:cs_drift_state('136f3e53-fd2f-41f4-bfdf-244802178c25', contemporary_post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('136f3e53-fd2f-41f4-bfdf-244802178c25', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, internationally_recognized_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, internationally_recognized_palestine).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, displaced_palestinians_1948_forward).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlers_beyond_partition_line).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_settlers_beyond_partition_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives legitimate statehood and recognized territorial sovereignty from the partition framework. The state apparatus, military, and legal system are organized around the partition principle (defined borders, fixed extent). Simultaneously operates as the enforcement actor maintaining those boundaries and expanding beyond them through settlements. Cannot exit the partition frame without losing the legitimacy its statehood derives from; trapped by its own founding principle.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, internationally_recognized_israel, beneficiary,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, internationally_recognized_israel, agenda_setter).

% Receives legitimate statehood claim through the partition resolution, granting diplomatic standing and UN recognition. However, statehood is nominally held without territorial contiguity, effective military control, or complete sovereignty. The population and leadership are identity-locked to Palestinian nationalism and to the partition-granted state structure as the only vehicle for self-determination. Cannot exit the partition frame without abandoning the statehood claim itself; cannot enforce the partition boundaries without military capacity it lacks.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, internationally_recognized_palestine, beneficiary,
    moderate, civilizational, identity_locked, national).

% Expelled or fled from their homes during the 1948 war and afterward; remain in refugee camps and diaspora without right of return under the partition framework. The partition resolution legitimates the Israeli state but provides no mechanism for addressing Palestinian displacement or restoring their property and territorial claims. They are the cost side of the partition arrangement: the extraction mechanism by which two states are created is their permanent exile. Trapped by geography, international law, and the absence of return rights; no exit option other than statelessness or acceptance of permanent refugee status.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, displaced_palestinians_1948_forward, payer,
    powerless, civilizational, trapped, regional).

% Establish permanent settlements in territories beyond the 1948 partition boundary (West Bank, East Jerusalem, Golan Heights). Under the strict partition reading, their settlements are illegitimate — they violate the fixed boundary the partition frame establishes. They benefit from security guarantees, resource access, and subsidized development from the Israeli state. They pay through exposure to international delegitimization, legal challenges to settlement validity, potential removal, and presence in a gray zone where the partition frame does not extend legitimacy. Constrained exit: cannot remain without accepting illegitimacy; cannot leave without abandoning property and community.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlers_beyond_partition_line, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_settlers_beyond_partition_line, beneficiary).

% Established the partition framework through UN Resolution 181 and continues to recognize both states, nominally endorsing the partition boundaries. Enforces the partition logic selectively: recognizes Israel within and beyond the partition line, nominally recognizes Palestine but tolerates Israeli occupation and settlement expansion beyond the partition boundary. Acts as the authoritative body that can reaffirm or revise the partition frame. The selective enforcement creates the gap where extraction becomes visible — the partition principle is invoked but not uniformly applied.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, united_nations_and_international_community, agenda_setter,
    institutional, generational, analytical, universal).

% Neighboring states (Egypt, Jordan, Syria, Lebanon) and non-state actors (Palestinian armed groups, Israeli far-right) are structurally implicated in the partition enforcement but excluded from boundary-setting. They advocate alternative legitimacy frames (security necessity for Israeli state, anti-colonial self-determination for Palestinians, regional security balancing for neighbors). Their exclusion from the initial partition resolution is itself an extraction mechanism: they cannot revise the boundaries through the partition frame but can only challenge them militarily or through non-state action. Trapped by the existing partition framework that does not admit their legitimacy claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, regional_security_actors, excluded,
    institutional, generational, trapped, regional).

% Analyze the coherence and application of the partition principle across time. Can identify the gap between the partition rule (fixed borders via UN decision) and its implementation (selective enforcement, settlement expansion, occupation, displacement persistence). Measure the theater ratio by tracking invocation of partition law versus invocation of security necessity in official justifications. Predict classification outcomes based on per-seat structural positions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, internationally_recognized_israel).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a territorial dispute by fixing boundaries through international law: both populations receive recognized statehood and territorial sovereignty within defined borders, ending competing absolutist territorial claims through binary partition rather than leaving territorial extent ambiguous or subject to military outcome.
% TRANSFER_FUNCTION: Moves legitimacy and sovereign authority from the British Mandate administration to two recognized nation-states; simultaneously concentrates territorial control in the hands of those who can enforce partition boundaries, extracting it from those displaced by the partition and from those who settle beyond the partition line (whose territorial claims are rendered illegitimate by the partition principle).
% ABSENT_VOICES: Palestinian refugees and their descendants, displaced in 1948 and after, are not present as parties to the partition arrangement despite bearing its primary cost. Regional security actors (neighboring states, non-state armed groups) advocating alternative legitimacy frames (security necessity, anti-colonial self-determination, indigenous continuity) are excluded from the partition boundary-setting but remain structurally implicated in its enforcement and contestation.
% DISAPPEARANCE_RATIONALE: If the partition resolution and its legitimacy frame disappeared overnight, the territorial arrangement would not revert to the Mandate — it would reorganize around whichever legitimacy frame could command military and political force at that moment. The security-necessity reading would likely escalate (military control becomes the primary justification for borders), or the indigenous-continuity reading would intensify (anti-colonial claims and refugee return movements would surge). The partition frame is what holds the specific UN-established border configuration in place; removing it removes the legal anchor and allows competing legitimacy frames to contest the territorial arrangement directly.
% FOUNDING_PROBLEM: How to resolve competing territorial claims of two populations over the same land when neither will voluntarily cede full control: partition offers a formal legal mechanism (UN recognition) to split territory and grant both populations statehood, avoiding winner-take-all military outcome and allowing coexistence through fixed boundaries.
% FOUNDING_PROBLEM_CORROBORATION: UN partition planners and international legal scholars attest the founding problem was to create a legal mechanism for coexistent statehood. Palestinian historians and liberation scholars attest the founding problem was framed in ways that excluded Palestinian refugee interests from the solution and treated Palestinian displacement as acceptable cost. Israeli security analysts attest the founding problem (coexistent statehood without military domination) remains unsolved and unresolvable through partition alone because security threats require variable territorial control. Regional actors attest the founding problem was never truly a partition problem — it was always a power problem (who controls the territory) that partition law cannot settle. External corroboration: post-1948 scholarly consensus (not from benefiting parties) is that the partition framework solved the legal problem (creating two recognized states) but failed to solve the political problem (both populations claiming the same territory) and the humanitarian problem (refugees displaced without right of return).
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58→0.67 over the interval) because the partition mechanism creates legitimate statehood for both parties but accomplishes this by displacing a third population (Palestinians who fled or were expelled in 1948 and after). The extraction is not from either state to the other, but from those displaced to the international legal system that recognizes both states. Suppression is high (0.55→0.71) because enforcement of the partition boundary requires continuous military/legal action to prevent (a) Palestinian resistance to territorial loss, (b) Israeli expansion beyond the partition line, and (c) refugee return claims. Theater is moderate and rising (0.25→0.44): the partition frame is invoked as the legitimacy principle, but actual enforcement is increasingly driven by security necessity (occupation, settlement expansion, military control) rather than by partition logic itself. The rise in theater_ratio from 1967 onward reflects the gap widening between the partition principle (fixed borders via UN decision) and the security-necessity principle (variable borders via military control) — the partition frame becomes more performative as security justifications dominate. Suppression is relatively stable despite the theater rise because the enforcement machinery persists even as its justification shifts.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli institutional seat, the partition reading legitimates the state and permits security-driven boundary adjustments for defensive purposes — the security necessity reading flows naturally from the initial partition grant. From the Palestinian moderate-power seat, the partition reading is simultaneously the only vehicle for statehood AND the mechanism that displaced 700,000+ people with no right of return — the indigenous-continuity reading contests whether partition can ever be legitimate when it erases indigenous presence. From the displaced-Palestinian powerless seat, the partition reading is pure extraction — legitimacy for others at the cost of permanent exile. The engine should compute a mountain-like classification from the Israeli security-setter position (extraction justified by partition + security necessity, high accessibility collapse, low resistance from within the framework), a snare-like classification from the powerless-displaced position (extraction with no coordination benefit), and a contested intermediate for the moderate Palestinian institutional position (nominal legitimacy without effective statehood). The same constraint produces different types per seat because the structural positions differ — beneficiary, payer, and excluded status are not uniform.
 *
 * DIRECTIONALITY LOGIC:
 *   The partition reading legitimates both Israel and Palestine as beneficiaries, but the beneficiary relationship is asymmetric: Israel consolidates military-enforced control within the partition boundary and beyond; Palestine receives nominal recognition without territorial contiguity or control. Directionality should show: Israel near the beneficiary end (d~0.2: benefits from partition recognition + selective boundary enforcement), Palestinians closer to symmetric (d~0.5: benefits from the partition principle that grants statehood but pays through exposure to military occupation and unfulfilled borders), displaced Palestinians at the full-target end (d~0.95: no benefits, full extraction via displacement), Israeli settlers at the payer end (d~0.75: pay through illegitimacy under the partition frame, constrained exit, but benefit from security/resource access in occupied zones). The international community sits as analytical (d~0.5). Directionality overrides are not needed if the beneficiary/victim declarations and exit_options are accurate; the derivation chain should produce the asymmetry automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to resolve competing territorial claims through legal partition) remains contested — its status is live for international lawyers citing UN legitimacy, dead for Palestinians experiencing ongoing occupation, and contested by security analysts who read the situation as requiring military control rather than legal borders. The disappearance verdict is world_rearranges: if the partition frame vanished, territorial authority would reorganize around whichever legitimacy frame could command force (likely security-necessity framing escalates, indigenous-continuity demands escalate). This mismatch (contested founding-problem status + world_rearranges verdict) flags mandatrophy risk: the arrangement persists but the problem it was built for remains unresolved and increasingly abstracted. The theater rise (partition frame invoked but security-necessity drives enforcement) indicates theatrical maintenance of the partition principle while the actual coordination mechanism shifts to security-driven control. A constraint that invokes partition law but enforces via occupation is operating in a mandatrophy state: the founding justification (partition = coordination) diverges from the operating mechanism (military = control). However, this is not mandatrophy_resolved because the partition frame remains active (not replaced) — it is mandatrophy_emergent, where the founding problem persists and the arrangement no longer solves it, but the extraction infrastructure remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_security_boundary_primacy,
    'Is the 1948 partition boundary the legitimate territorial extent of both states, or is the 1967 boundary (following military control) the legitimate extent? Which legitimacy frame is primary?',
    'Resolution would require binding international legal judgment (International Court of Justice, UN Security Council resolution with enforcement) on whether the partition frame supersedes the security-necessity frame, or vice versa. Currently, both are invoked selectively.',
    'If partition is primary, Israeli settlements beyond 1967 lines and occupation are illegitimate; Palestinian statehood is complete within partition boundaries with right of return for refugees. If security-necessity is primary, the 1967 lines and beyond can be legitimate; Palestinian statehood is renegotiated. If they coexist (both frames live), the constraint remains tangled-rope with unresolved extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_vs_security_boundary_primacy, conceptual, 'Which legitimacy frame (partition law vs. security necessity) is structurally primary in resolving the territorial dispute.').

omega_variable(
    extraction_vs_coordination_in_partition,
    'Is the displacement of Palestinians in 1948 and after a necessary cost of the coordination (partition solving the territorial dispute) or a separate extraction mechanism that rides on the partition frame?',
    'Counterfactual analysis: could a partition have been accomplished that satisfied both populations'' territorial claims without displacement? If yes, displacement is a separate extraction; if no, it is embedded in the coordination cost.',
    'If displacement is necessary, the extraction is lower and the constraint moves toward rope. If displacement is separate, the extraction is higher (current high measurement stands) and the constraint remains snare-flavored for the displaced population. Classification differs per seat regardless (seats experience the partition coordination and the displacement extraction differently).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_in_partition, empirical, 'Whether Palestinian displacement is a necessary cost of partition or an extractive mechanism separable from partition coordination.').

omega_variable(
    international_recognition_authority_source,
    'Does the partition frame derive its legitimacy from (a) the UN as a neutral arbiter of territorial disputes, (b) the populations'' consent to the partition (which was never unanimous), or (c) great-power enforcement (colonial mandate termination)?',
    'Historical and philosophical analysis of which authority grounding the partition reads on. Affects whether the partition frame is self-authenticating or derivative.',
    'If (a), the partition is universal-scope arbitration and the legitimacy is robust. If (b), consent was asymmetric and the legitimacy is contested (Palestinian consent was not secured). If (c), the legitimacy is derivative from power, not autonomous. Each grounds a different cs_authority_grounding atom and affects how the indigenous-continuity reading contests the partition authority itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_authority_source, conceptual, 'What authority grounds the partition frame''s legitimacy claim.').

omega_variable(
    refugee_right_of_return_structural_role,
    'Is the right of return for Palestinian refugees a component of the partition-frame legitimacy (both states legitimate only if return rights are honored) or an external claim that contests the partition frame?',
    'Reading of UN Resolution 181 and subsequent international law instruments. Does the partition framework itself mandate return rights, or are return rights a demand of those who reject the partition?',
    'If return is integral to partition legitimacy, the constraint includes return as a beneficiary entitlement and the displacement is not extraction but unfulfilled coordination. If return is external, displacement is pure extraction from the partition frame''s perspective. Classification of the constraint shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_right_of_return_structural_role, conceptual, 'Whether refugee right of return is interior or exterior to the partition-frame legitimacy claim.').

omega_variable(
    kernel_reading_contest_structural,
    'Do the three readings of the territorial_legitimacy kernel (partition, security-necessity, indigenous-continuity) coexist as different factions'' live positions, or does one reading logically foreclose the others?',
    'Examine the axioms of each reading: if the core premises contradict (partition says legitimacy is via UN recognition; security-necessity says legitimacy is via control capability; indigenous-continuity says legitimacy is via unbroken habitation), then foreclosure is present. If each reading''s axioms are consistent within their own framework but incompatible across frameworks, then coexistence is the structural fact.',
    'If coexistence: the constraint remains a contested tangled_rope with per-seat classification divergence. If foreclosure: one reading''s terminal state eliminates the others as possibilities in the same institutional framework. Currently, all three readings remain live across different factions, suggesting coexistence rather than foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structural, conceptual, 'Whether the three readings of territorial legitimacy foreclose each other or coexist as live contested positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.32).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__partition_reading, theater_ratio, 1987, 0.39).
narrative_ontology:measurement_basis(terr_tr_t1987, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__partition_reading, theater_ratio, 2000, 0.43).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2012, territorial_legitimacy__partition_reading, theater_ratio, 2012, 0.45).
narrative_ontology:measurement_basis(terr_tr_t2012, observed).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.44).
narrative_ontology:measurement_basis(terr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.58).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.61).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__partition_reading, base_extractiveness, 1987, 0.64).
narrative_ontology:measurement_basis(terr_be_t1987, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__partition_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2012, territorial_legitimacy__partition_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement_basis(terr_be_t2012, observed).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.67).
narrative_ontology:measurement_basis(terr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.68).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__partition_reading, suppression_requirement, 1987, 0.72).
narrative_ontology:measurement_basis(terr_su_t1987, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__partition_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2012, territorial_legitimacy__partition_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement_basis(terr_su_t2012, observed).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(terr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__partition_reading, 0.15).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel is instantiated as three constraint stories, each representing a different reading of the same commitment to territorial authority in the Israeli-Palestinian context. The partition_reading (this file) treats legitimacy as conferred by UN Resolution 181 and international recognition. The security_necessity_reading treats legitimacy as derived from capacity to provide security and control territory (1967 borders plus strategic depth). The indigenous_continuity_reading treats legitimacy as rooted in unbroken indigenous habitation and anti-colonial self-determination. Each reading produces a different ε-value, beneficiary/victim structure, and per-seat classification. They are linked because they compete to frame the same territorial dispute; reading adoption or rejection by factions shifts the instantiated constraint from partition to security or indigenous framing. The partition reading influences both siblings because the partition framework established the initial boundary that security-necessity subsequently contests (1967 beyond 1948) and that indigenous-continuity reads as a colonial imposition. These are not perspectives on one constraint; they are three constraints that share a kernel and compete for institutional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, powerless, 0.95).
constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

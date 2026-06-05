% ============================================================================
% CONSTRAINT STORY: declaration_of_rights_1789__universal_charter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_declaration_of_rights_1789__universal_charter_reading, []).

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
 *   constraint_id: declaration_of_rights_1789__universal_charter_reading
 *   human_readable: Declaration of Rights 1789: Universal Charter Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The Declaration of Rights 1789 instantiates one specific reading of a
 *   contested kernel: the universal charter reading asserts that the
 *   Declaration spoke for all mankind, transcending French particularity to
 *   establish a legitimacy claim valid across all peoples and borders. This
 *   reading suppresses particularist rights traditions (English common law,
 *   guild rights, customary law, cultural protections) by reframing them as
 *   mere particularisms rather than alternative grounds for rights. The
 *   constraint exhibits tangled coordination and extraction: genuine
 *   coordination function (enabling rational rights agreement across cultural
 *   boundaries) coupled with asymmetric extraction (concentrating the power
 *   to define legitimate rights claims in universalist frames, suppressing
 *   alternatives). The enforcer (the Revolutionary French Republic)
 *   experienced this as both coordination (uniting diverse groups under
 *   shared principles) and extraction (requiring terror to suppress
 *   particular traditions). Over 230 years, the theater ratio has risen as
 *   interpretive institutions have performed increasing reinterpretive labor
 *   to sustain the Declaration's universal claim against historical and
 *   contemporary rights assertions that the text does not contain (labor
 *   rights, social rights, reproductive autonomy, indigenous rights,
 *   environmental rights). The suppression_requirement has declined as
 *   post-colonial and pluralist frameworks have carved out space for
 *   particularisms within a (modified) universalist order. This reading
 *   coexists with two siblings: the bourgeois property charter reading (which
 *   highlights Article 17's sacred property protections) and the declaratory
 *   unenforceable reading (which emphasizes the gap between declaration and
 *   enforcement). These readings occupy different institutional and
 *   intellectual positions rather than directly refuting one another.
 *
 * KEY AGENTS:
 *   - Universalist Legitimacy Regime (institutional/arbitrage): Primary beneficiary. The universal frame grants global legitimacy and institutional substance to rights claims grounded in universal principles rather than particular traditions.
 *   - Subsequent Importer Nations (institutional/constrained): Derivative beneficiary. Access to universalist legitimacy enables rights claims but constrains them to universalist vocabulary; must reframe particular traditions as instantiations of universal rights.
 *   - Particularist Rights Traditions (powerless/trapped): Primary victim. English common law historicism, guild rights, customary law protections, and cultural particularisms are delegitimized as 'mere particularisms' once the universal charter asserts primacy.
 *   - Revolutionary French Republic (institutional/constrained): Enforcer and partial victim. Coordinates diverse groups through universalism but extracts suppression of particular French traditions (noble privileges, clerical exemptions, regional franchises). Constrained by revolutionary logic that requires enforcing universalism through terror.
 *   - Interpretive Institutional Tradition (institutional/arbitrage): Theater maintainer. Scholarly, diplomatic, and legal institutions sustain the Declaration's authority through reinterpretive labor, adapting the text to accommodate rights it does not contain. Theater persists because the authority is real but the textual basis is narrow.
 *   - Post-Colonial Rights Movements (organized/constrained): Scaffold users. Leverage the Declaration's universalist language to contest colonialism but move toward frameworks that accommodate pluralism alongside universalism, gradually climbing past the Declaration's monopoly.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(declaration_of_rights_1789__universal_charter_reading, 0.58).
domain_priors:suppression_score(declaration_of_rights_1789__universal_charter_reading, 0.62).
domain_priors:theater_ratio(declaration_of_rights_1789__universal_charter_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(declaration_of_rights_1789__universal_charter_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(declaration_of_rights_1789__universal_charter_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(declaration_of_rights_1789__universal_charter_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(declaration_of_rights_1789__universal_charter_reading, tangled_rope).
narrative_ontology:human_readable(declaration_of_rights_1789__universal_charter_reading, "Declaration of Rights 1789: Universal Charter Reading").
narrative_ontology:topic_domain(declaration_of_rights_1789__universal_charter_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(declaration_of_rights_1789__universal_charter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(declaration_of_rights_1789__universal_charter_reading, 'c64a2690-e806-4785-b9b3-7382be2cd89a').
narrative_ontology:cs_kernel_codification('c64a2690-e806-4785-b9b3-7382be2cd89a', fixed_text).
narrative_ontology:cs_authority_grounding('c64a2690-e806-4785-b9b3-7382be2cd89a', extraction).
narrative_ontology:cs_interpretation_layer_present('c64a2690-e806-4785-b9b3-7382be2cd89a').
narrative_ontology:cs_reading_relation('c64a2690-e806-4785-b9b3-7382be2cd89a', declaration_of_rights_1789__bourgeois_property_charter_reading, coexists_with).
narrative_ontology:cs_reading_relation('c64a2690-e806-4785-b9b3-7382be2cd89a', declaration_of_rights_1789__declaratory_unenforceable_reading, coexists_with).
narrative_ontology:cs_axiom('c64a2690-e806-4785-b9b3-7382be2cd89a', foundational, rights_are_universal_to_mankind).
narrative_ontology:cs_axiom_status(rights_are_universal_to_mankind, holdable).
narrative_ontology:cs_axiom_grounding('c64a2690-e806-4785-b9b3-7382be2cd89a', rights_are_universal_to_mankind, deontological).
narrative_ontology:cs_axiom('c64a2690-e806-4785-b9b3-7382be2cd89a', secondary, universalism_suppresses_particularisms_as_necessary).
narrative_ontology:cs_axiom_status(universalism_suppresses_particularisms_as_necessary, holdable).
narrative_ontology:cs_axiom_grounding('c64a2690-e806-4785-b9b3-7382be2cd89a', universalism_suppresses_particularisms_as_necessary, deontological).
narrative_ontology:cs_reference_frame('c64a2690-e806-4785-b9b3-7382be2cd89a', universal_inherent_rights_transcending_nationality).
narrative_ontology:cs_drift_state('c64a2690-e806-4785-b9b3-7382be2cd89a', contemporary_post_colonial_pluralist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c64a2690-e806-4785-b9b3-7382be2cd89a', '').
narrative_ontology:cs_kernel_id(declaration_of_rights_1789__universal_charter_reading, declaration_of_rights_1789).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(declaration_of_rights_1789__universal_charter_reading, universalist_legitimacy_claim).
narrative_ontology:constraint_beneficiary(declaration_of_rights_1789__universal_charter_reading, subsequent_importer_nations).
narrative_ontology:constraint_victim(declaration_of_rights_1789__universal_charter_reading, particularist_rights_traditions).
narrative_ontology:constraint_victim(declaration_of_rights_1789__universal_charter_reading, english_common_law_historicism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARTICULARIST RIGHTS TRADITIONS (SNARE) — Trapped by the Declaration's universalist frame. Pre-existing rights-of-Englishmen, customary law traditions, and particular cultural protections are foreclosed as 'mere particularisms' once the universal charter asserts its legitimacy globally. No exit from the suppression of local rights frameworks. Maximum experienced extraction — the constraint actively delegitimizes alternative grounds for rights claims.
constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IMPORTING NATIONS (TANGLED ROPE) — Constrained by the need to justify rights in universal language (the Declaration's frame) rather than their own traditions. But they also benefit from the Declaration's legitimacy — adopting it grants access to a global epistemic commons and external validation. They experience both coordination (access to universal rights language) and extraction (pressure to suppress local traditions in favor of universalist framing). Constrained exit — rejecting the universal frame risks diplomatic isolation and epistemic marginalization.
constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSALIST LEGITIMACY REGIME (ROPE) — Pure coordination from the standpoint of universalist commitments. The Declaration establishes a framework enabling rational agreement across borders by abstracting from particular traditions. The regime benefits from the constraint — universalism gains institutional substance and enforceability through the Declaration's global reach. Arbitrage available through reinterpreting particular traditions as instantiations of universal principles. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REVOLUTIONARY FRENCH REPUBLIC (TANGLED ROPE) — The enforcer of the universal claim carries both coordination and extraction burdens. Genuine coordination function: the Declaration enables France to unite diverse social strata under shared universal principles rather than traditional royal prerogative. But also extraction: the universalist frame suppresses rival French traditions (guild rights, regional privileges, clerical exemptions, women's existing customary protections) in the name of universal man. Constrained exit — abandoning universalism would dissolve the revolutionary legitimacy claim, but enforcing it required terror to suppress particularisms. The regime experience is deeply mixed.
constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERPRETIVE INSTITUTIONAL TRADITION (PITON) — The scholarly and diplomatic apparatus that maintains the Declaration's universalist reading through interpretive labor. Much of this maintenance is performative: reinterpreting the Declaration to align with contemporary rights (labor rights, social rights, reproductive rights) that the 1789 text does not contain, yet claiming fidelity to its 'universal' principles. The theater persists because the Declaration's authority is real, but the interpretive work required to sustain its universal claim against the text's actual content is substantial. Theater ratio captures this gap between the claim of universal timelessness and the active reinterpretation required to maintain it.
constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-COLONIAL RIGHTS MOVEMENTS (SCAFFOLD) — Organized agents leveraging the Declaration's universal language to contest colonialism and demand decolonization. The constraint has a sunset clause from this perspective: as movements embed indigenous and postcolonial rights frameworks into international law (UNDRIP, decolonial theory), the Declaration's monopoly on universalist legitimacy erodes. The Declaration functions as a temporary ladder enabling rights claims but is being climbed past — the universalist frame that once suppressed particularisms is being superseded by frameworks that accommodate cultural pluralism while maintaining universalist aspirations. Constrained but with visible exit path.
constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, the universalist framing appears as a discovery of natural law: that human rights are inherent to mankind and not contingent on citizenship, tradition, or state grant. This perspective sees the Declaration as revealing an immutable moral order that transcends particular legal systems. However, the structural data contradicts the mountain classification — identifiable beneficiaries (universalist regimes, subsequent importers), identifiable victims (suppressed particularist traditions), and active enforcement requirements reveal this as a false summit: the natural-law appearance naturalizes a contingent institutional and doctrinal choice about which grounds for rights claims will be recognized as legitimate.
constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(declaration_of_rights_1789__universal_charter_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(declaration_of_rights_1789__universal_charter_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(declaration_of_rights_1789__universal_charter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(declaration_of_rights_1789__universal_charter_reading, TR),
    TR >= 0.70.

:- end_tests(declaration_of_rights_1789__universal_charter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Declaration's universalist frame extracts benefit by monopolizing legitimate rights discourse — other traditions must justify themselves in universalist language or be dismissed as particularisms. But the extraction is partial, not total: the universalist frame genuinely expanded rights claims beyond what many traditional systems allowed. The 0.58 value reflects substantial extraction with offsetting coordination benefit. The measurement trajectory shows rising extractiveness over time (0.35 → 0.58) as the interpretation layer required to sustain universalism has thickened, suggesting the Declaration absorbs new rights claims through reinterpretation rather than textual expansion, increasing the theater required. Suppression (0.62): Moderate-high. The suppression of particularist traditions is significant — guild rights, customary law protections, and regional privileges were actively suppressed in the Revolution's enforcement. But the suppression is not total: some particularisms (property, community, family structures) are incorporated into the universal frame rather than eliminated. The declining trajectory (0.75 → 0.62) reflects the post-colonial and pluralist challenge to universalist hegemony. Theater ratio (0.65): Moderate-high. Contemporary application of the Declaration to rights it does not textually contain (labor, social, reproductive, environmental) requires substantial reinterpretive labor. The Declaration names a narrow set of rights (property, liberty, security, resistance); 230 years of interpretation have read it as implying a far broader set. The rising trajectory (0.45 → 0.65) reflects increasing theater as the gap between text and interpretation widens. Claimed type (tangled_rope): Justified by the presence of genuine coordination (enabling rational agreement across culturally diverse groups) alongside asymmetric extraction (concentrating rights-definition power in universalist frames, suppressing alternatives).
 *
 * PERSPECTIVAL GAP:
 *   The constraint displays a full range of DR classifications reflecting genuine structural differences in agents' positions. The suppressed particularist traditions experience snare (trapped, powerless, no exit) because universalism forecloses their legitimacy as alternative grounds for rights. Importing nations experience tangled rope (constrained, institutional) because they benefit from universalist legitimacy while being pressured to abandon particular traditions. The universalist regime experiences pure rope (arbitrage, institutional) — genuine coordination function, no experienced extraction. The enforcer experiences tangled rope (constrained, institutional) — coordination benefit but coercive suppression required. The interpretive apparatus experiences piton (arbitrary, institutional) — authority is real, but theater required to maintain it. Post-colonial movements experience scaffold (organized, constrained) — leveraging universalism to contest colonialism but building toward post-universalist frameworks with sunset on the Declaration's monopoly. The analytical observer risks mountain (natural law) — seeing universalism as a discovery of inherent rights — but the structural data reveals false summit: identifiable beneficiaries, victims, and enforcement requirements show this is a contingent institutional choice about which grounds for rights will be recognized as legitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) flows from the agent's structural position relative to the constraint. Suppressed traditions: high d (0.90+) — pure target, no beneficiary role, trapped exit → maximum f(d) → high experienced extraction χ. Importing nations: moderate d (0.55-0.65) — both beneficiary (access to universalist legitimacy) and victim (pressure to abandon particularisms), constrained exit → moderate f(d) → moderate χ. Universalist regime: low d (0.10-0.20) — pure beneficiary, arbitrage exit → negative f(d) → negative χ or small positive. Enforcer: moderate-high d (0.60-0.70) — enforcer benefits but also suppresses, constrained exit → moderate-high f(d) → moderate-high χ. Interpretive apparatus: low d (0.15-0.25) — benefits from Declaration's authority, arbitrage through reinterpretation → negative f(d) → no experienced extraction. Post-colonial movements: moderate d (0.50-0.60) — both users of universalist frame (benefit) and challengers to it (victim of monopoly), constrained with visible exit → moderate f(d) → moderate χ. Analytical observer: high d (0.70+) — positioned to see the full structure, analytical exit → high f(d) under legacy mapping, but modern analytical d is context-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all six types are legitimate readings of the constraint from different structural positions, but the central analytical question is whether the Declaration's universalism coordinates genuine rational agreement or extracts legitimacy by suppressing alternatives. The tangled rope classification reflects substantial extraction (0.58) with genuine coordination function (right-to-rights across borders without shared tradition). The false summit mountain (natural law) is diagnosed by the structural data: beneficiaries (universalist regimes), victims (suppressed traditions), enforcement requirements (Revolutionary terror, interpretive institutions) reveal this is not a natural law but a contingent institutional choice. The piton classification of the interpretive tradition reveals that much of the Declaration's contemporary authority depends on performative reinterpretation — the theater is rising as the text's limitations become more visible. The scaffold classification of post-colonial movements indicates a real exit path: new frameworks (UNDRIP, decolonial theory, indigenous rights) are building alternatives to the Declaration's universalist monopoly. The mandatrophy demands clarity on this question: Is universalism a discovery of natural rights (mountain) or a contingent institutional arrangement that extracts benefit by suppressing alternatives (tangled rope/snare)? The structural data supports the tangled rope reading: genuine coordination function paired with asymmetric extraction and active enforcement, with rising theater as the interpretation layer required to sustain the claim thickens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_particular_incommensurability,
    'Is the tension between universal rights and particular traditions a logical contradiction to be resolved by foreclosure, or a structural coexistence that different frameworks navigate differently?',
    'Examination of cases where particular rights traditions are NOT subordinated to universal frames (e.g., indigenous rights recognized alongside universal rights); determination of whether such coexistence requires reframing universalism or abandoning it',
    'If truly incommensurable: universalist reading FORECLOSES particularist traditions (strong claim, rare). If coexistable: readings can coexist with different institutional positions choosing different frames (weaker claim, more common). Impact on classification: foreclosure → universalist reading is stronger claim; coexistence → readings are genuinely competitive options held by different parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_vs_particular_incommensurability, conceptual, 'Whether universal and particular rights frameworks are logically incompatible or structurally coexistent').

omega_variable(
    extractiveness_of_legitimacy_globalization,
    'Does the Declaration''s globalization of legitimacy claims extract benefit by concentrating the power to define ''rights'' in universalist frames, or does it genuinely enable broader rights claims than particularist systems allowed?',
    'Comparative analysis of rights expansion: count of newly asserted rights claims enabled by universalist frame vs. suppressed historical rights claims. Timeline of rights recognition (labor, social, reproductive, environmental) vs. timeline of suppression of particular traditions (guild privileges, regional exemptions, customary law). Measurement of enforcement asymmetry: are universal rights enforced against powerful actors? Are particularist claims suppressed even when politically salient?',
    'If expansion >> suppression: extractiveness closer to 0.30 (net coordination with extraction cost). If suppression >> expansion: extractiveness closer to 0.70+ (net extraction hidden by universalist framing). Current estimate (0.58) reflects substantial mixed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_of_legitimacy_globalization, empirical, 'Whether universalist legitimation extracts value by concentrating rights-definition power').

omega_variable(
    enforcement_capacity_and_theater_ratio,
    'To what extent does the Declaration''s enforcement capacity depend on active institutional reinterpretation to sustain the universal claim, vs. the text genuinely supporting 18th-century rights as universal?',
    'Textual analysis of what rights the Declaration actually names (property, liberty, security, resistance to oppression) vs. what later interpretations have added (labor rights, social rights, reproductive autonomy). Measurement of interpretive labor required in contemporary usage. Assessment of whether the text admits the interpretation or requires it.',
    'If text sustains broad reading: theater_ratio lower (~0.40-0.50), claim is grounded. If text requires reinterpretation: theater_ratio higher (~0.70+), maintenance is performative. Current estimate (0.65) reflects substantial reinterpretive labor required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_and_theater_ratio, empirical, 'How much of the Declaration''s contemporary authority depends on interpretive theater vs. textual content').

omega_variable(
    suppression_mechanism_doctrine_vs_enforcement,
    'Is the suppression of particularist traditions a doctrinal consequence (the universal frame logically forecloses particularisms) or an enforcement consequence (the French republic actively suppressed particular rights in the Revolution''s name)?',
    'Historical analysis separating the Declaration''s doctrinal content from the revolutionary enforcement apparatus. Examination of whether rejecting particularist traditions is required by the universal frame or by political choices made in enforcing universalism. Assessment of alternative framings that might accommodate both universal and particular rights.',
    'If purely doctrinal: the reading is more about logical structure. If partly enforcement-dependent: the suppression is a contingent historical choice, not inherent to universalism. Affects axiom status: doctrine_suppression_necessary vs. enforcement_suppression_contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_doctrine_vs_enforcement, conceptual, 'Whether suppression of particularisms is logically required or historically contingent').

omega_variable(
    legitimacy_authority_erosion,
    'Is the Declaration''s universalist authority eroding over time as post-colonial, feminist, and indigenous rights frameworks assert alternatives, or is the Declaration absorbing these movements through reinterpretation (interpretation_layer_present = true)?',
    'Analysis of contemporary rights movements: do they explicitly cite the Declaration as foundational or explicitly reject it? Measurement of the Declaration''s role in new rights frameworks. Assessment of whether newer frameworks are positioned as extensions of the Declaration''s universalism or alternatives to it.',
    'If eroding: reference_frame is drifting toward authority_erosion, magnitude substantial or severe. If absorbing: interpretation layer is buffering the core authority, drift is minor and managed. Current assessment: substantial erosion with active reinterpretation by interpretive institutions (the piton''s theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_authority_erosion, empirical, 'Whether the Declaration''s universalist authority is eroding or being sustained through reinterpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(declaration_of_rights_1789__universal_charter_reading, 0, 230).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decl_univ_tr_t0, declaration_of_rights_1789__universal_charter_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(decl_univ_tr_t50, declaration_of_rights_1789__universal_charter_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(decl_univ_tr_t230, declaration_of_rights_1789__universal_charter_reading, theater_ratio, 230, 0.65).

% Extraction over time
narrative_ontology:measurement(decl_univ_be_t0, declaration_of_rights_1789__universal_charter_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(decl_univ_be_t50, declaration_of_rights_1789__universal_charter_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(decl_univ_be_t230, declaration_of_rights_1789__universal_charter_reading, base_extractiveness, 230, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(decl_univ_su_t0, declaration_of_rights_1789__universal_charter_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(decl_univ_su_t50, declaration_of_rights_1789__universal_charter_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(decl_univ_su_t230, declaration_of_rights_1789__universal_charter_reading, suppression_requirement, 230, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(declaration_of_rights_1789__universal_charter_reading, identity_coordination).
narrative_ontology:affects_constraint(declaration_of_rights_1789__universal_charter_reading, declaration_of_rights_1789__bourgeois_property_charter_reading).
narrative_ontology:affects_constraint(declaration_of_rights_1789__universal_charter_reading, declaration_of_rights_1789__declaratory_unenforceable_reading).

% DUAL FORMULATION NOTE:
% The declaration_of_rights_1789 kernel has three structurally distinct readings, each with different ε values reflecting different observable dimensions of the Declaration's function. The universal_charter_reading (this story) focuses on the Declaration's legitimacy claim and suppression of particularisms (ε=0.58). The bourgeois_property_charter_reading focuses on Article 17's role in securing property (likely ε=0.35-0.45, Rope or Tangled Rope). The declaratory_unenforceable_reading focuses on the gap between declaration and enforcement (likely ε=0.65-0.75, Snare). Each reading observes the same text but identifies different structural functions and different beneficiary/victim sets. All three readings affect the Declaration's global normative authority; the reading that dominates contemporary discourse shapes how the constraint operates in importing nations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

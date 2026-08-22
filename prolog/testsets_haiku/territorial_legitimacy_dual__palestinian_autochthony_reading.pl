% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Territorial Legitimacy via Autochthony and Right of Return
 *   domain: political/territorial
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the territorial legitimacy
 *   kernel: Palestinian legitimacy grounded in continuous habitation,
 *   displacement trauma, and the right of return. Under this reading, the
 *   1948 displacement of Palestinian Arabs constitutes an ongoing injustice
 *   requiring remedy, not a historical event to be absorbed. The Palestinian
 *   people's claim to the territory rests on autochthony (they were there
 *   first and continuously), on collective trauma that demands reparation,
 *   and on the legal/moral norm that displacement should be reversible
 *   through return rights. The Israeli state, under this reading, lacks
 *   legitimate claim to all its current territory because that claim was
 *   built on dispossession. The constraint is the structural arrangement
 *   enforcing Palestinian territorial reduction and denying return — what
 *   this reading calls occupation, settlers call security necessity, and
 *   competing readings frame as compromise. This is ONE reading of a
 *   contested kernel; the sibling readings (zionist_refuge_reading,
 *   two_state_coexistence_reading) represent different parties' structural
 *   interpretation of the same underlying commitment (territorial
 *   legitimacy).
 *
 * KEY AGENTS:
 *   - Palestinian refugee diaspora: stateless, trauma-carrier, trapped exit
 *   - Palestinian territorial remainder: confined to 22% of historical territory, identity-locked to return claim
 *   - Israeli state apparatus: agenda-setter, enforcer, beneficiary via territorial control and demographic dominance
 *   - Jewish diaspora settlement movement: organized beneficiary, politically mobilized to prevent territorial concession
 *   - International community: fragmentary observer, rhetorically supporting Palestinian rights but militarily/economically supporting Israeli state
 *   - Palestinian political leadership: constrained middle seat, claims to represent diaspora and remainder but lacks enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.89).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.91).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Territorial Legitimacy via Autochthony and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '3352e2b8-00bd-4449-abd3-5f4c108a3e38').
narrative_ontology:cs_kernel_codification('3352e2b8-00bd-4449-abd3-5f4c108a3e38', distributed).
narrative_ontology:cs_authority_grounding('3352e2b8-00bd-4449-abd3-5f4c108a3e38', extraction).
narrative_ontology:cs_interpretation_layer_present('3352e2b8-00bd-4449-abd3-5f4c108a3e38').
narrative_ontology:cs_reading_relation('3352e2b8-00bd-4449-abd3-5f4c108a3e38', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('3352e2b8-00bd-4449-abd3-5f4c108a3e38', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('3352e2b8-00bd-4449-abd3-5f4c108a3e38', foundational, palestinian_autochthony_justifies_right_of_return).
narrative_ontology:cs_axiom_status(palestinian_autochthony_justifies_right_of_return, holdable).
narrative_ontology:cs_axiom_grounding('3352e2b8-00bd-4449-abd3-5f4c108a3e38', palestinian_autochthony_justifies_right_of_return, deontological).
narrative_ontology:cs_axiom('3352e2b8-00bd-4449-abd3-5f4c108a3e38', foundational, territorial_dispossession_requires_restitution_not_compromise).
narrative_ontology:cs_axiom_status(territorial_dispossession_requires_restitution_not_compromise, holdable).
narrative_ontology:cs_axiom_grounding('3352e2b8-00bd-4449-abd3-5f4c108a3e38', territorial_dispossession_requires_restitution_not_compromise, deontological).
narrative_ontology:cs_reference_frame('3352e2b8-00bd-4449-abd3-5f4c108a3e38', pre_1948_palestinian_territorial_integrity).
narrative_ontology:cs_drift_state('3352e2b8-00bd-4449-abd3-5f4c108a3e38', contemporary_occupation_hardening, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3352e2b8-00bd-4449-abd3-5f4c108a3e38', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_diaspora_settlement_movement).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_territorial_remainder).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carries intergenerational displacement trauma and lives in camps or host nations under restrictive legal status (stateless, right-of-return denied by military occupation, citizenship withheld by many Arab states). Bears the cost of territorial loss without compensation or return option. Exit would mean accepting permanent exile status or violent self-assertion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% Inhabits fragmented West Bank and Gaza, under military administration with restricted movement, resource extraction, and political sovereignty. Confined to 22% of historical Palestinian territory. Identity is bound to claim of territorial rights; exit means accepting permanent reduced status or departure.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_territorial_remainder, payer,
    moderate, generational, identity_locked, regional).

% Controls enforcement of the occupation, territorial administration, settlement expansion policy, and denial of return. Collects territorial control, resource access, security perimeter, and demographic advantage from the arrangement. Sets rules for Palestinian movement, construction, and political assembly. Frames the constraint as security necessity and legal entitlement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, beneficiary).

% Receives territorial access, property rights, and state subsidy for settlement in occupied lands. Settles on expropriated Palestinian land under military protection. Identity and material interest fused with occupation continuity; exit would mean evacuation and loss of property claims. Politically mobilized to resist any territorial concession.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_diaspora_settlement_movement, beneficiary,
    organized, generational, constrained, global).

% UN resolutions, humanitarian bodies, and majority-nation declarations support Palestinian statehood and right of return. US, EU, and other powers provide military/economic support to Israeli state while rhetorically acknowledging Palestinian rights. Enforcement authority is diffuse and contradictory; observers lack unified capacity to alter the constraint.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_community_fragmentary, observer,
    institutional, biographical, analytical, global).

% Negotiates under asymmetric power; claims to represent the refugee diaspora and territorial remainder but lacks enforcement capacity over either. Trapped between international pressure (two-state compromise), diaspora expectation (right of return), and Israeli military dominance. Limited to administering the territorial remainder under occupation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership, agenda_setter).

% The vindicated proposition that Jewish historical trauma justifies territorial refuge and state formation. This is not an agent but a normative claim embedded in Israeli legitimacy framing. It collects no material gain but is used to defend the constraint's existence and resist remedy frames that center Palestinian dispossession.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_historical_persecution_narrative, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_historical_persecution_narrative).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading does not recognize a coordination function in the constraint. The structure does not solve a genuine collective-action problem that would be unsolvable absent the constraint. Any purported coordination content (security, governance, demographic stability) is secondary to and justified by territorial control, not the other way around.
% TRANSFER_FUNCTION: Transfers Palestinian territorial rights, property ownership, water resources, and freedom of movement to Israeli state control. Transfers Palestinian labor (through permit-dependent employment), tax extraction, and settlement subsidies to Israeli benefit. Transfers statelessness, intergenerational trauma, and legal disability to Palestinian refugee diaspora. Transfers identity-fused territorial loss to Palestinian territorial remainder. The transfer is unidirectional with no reciprocal gain to Palestinian seats.
% ABSENT_VOICES: Palestinian refugees living outside the territorial remainder (Lebanon, Syria, Jordan, diaspora globally) have no formal institutional voice in the constraint's governance. They carry the strongest claim to remedy (right of return as the displaced party) and the least political power. Their exclusion from the table is structural to the arrangement — they cannot participate in Palestinian Authority politics, cannot vote in Israeli elections despite being affected by the constraint, and are systematically prevented from returning. Their absence is not accidental; it is engineered by the constraint itself.
% DISAPPEARANCE_RATIONALE: If this constraint (occupation, settlement, return denial) disappeared, the entire territorial order would reorganize. Palestinian refugees would have a legal basis and political pathway to return; Israeli state would negotiate borders with a Palestinian state; property disposition would be reopened; the regional balance of power would shift; demographic composition would change. The constraint's persistence requires active enforcement precisely because its disappearance would unwind the territorial arrangement itself.
% FOUNDING_PROBLEM: The founding problem (from the Israeli/Zionist reference frame that this reading disputes) is Jewish historical persecution culminating in the Holocaust and the need for a secure territorial refuge. This reading's counter-problem is Palestinian displacement and the need for territorial restitution and return.
% FOUNDING_PROBLEM_CORROBORATION: Israeli state, Zionist historical scholarship, and international Holocaust studies attest that Jewish refuge was a necessary response to the Holocaust and historical persecution — that founding problem was real and urgent. Palestinian scholars, international human rights bodies (Amnesty International, Human Rights Watch), historians outside the Israeli/Palestinian camps, and the UN majority declare that Jewish refuge was legitimate but the specific solution (territorial seizure from Palestinian Arabs) was not necessary and created a new problem (Palestinian dispossession) that now requires remedy. No neutral external party simply corroborates the Israeli founding problem without also noting the Palestinians' counter-claim. The corroboration is split: the founding problem is attested as real, but its solution is contested as illegitimate.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is highest among all constraint readings (0.89 at interval end) because this reading frames the arrangement as pure territorial seizure with no coordination component — no genuine problem solved for Palestinian seats, only deprivation imposed. Suppression is similarly extreme (0.91) because the constraint's persistence depends on continuous military enforcement, denial of return, and prevention of alternative political arrangements. Theater ratio rises over time (0.25 to 0.42) because the state increasingly relies on security rhetoric (terrorism prevention, demographic stability) to justify territorial exclusion, even as the foundational problem (Holocaust refuge necessity) recedes in direct justification. Accessibility collapse is high because Palestinians have no viable political exit once the constraint is understood — they can accept permanent statelessness, attempt violent disruption, or pursue emigration, but cannot negotiate a return within the current framework. Resistance rises over time (0.58 to 0.64) as diaspora and remainder mobilize separately: diaspora through BDS and advocacy; remainder through non-state actors and occasional organized uprising. Measurement series are aligned on a shared time grid covering 75 years (1948 to ~2023) to capture the constraint's historical trajectory from initial imposition through contemporary hardening.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (refugee diaspora, territorial remainder) should compute as experiencing pure snare with no coordination component. The beneficiary seats (Israeli state, settlement movement) should compute as experiencing justified coordination with security content (though that justification is contested). The observer seats should compute as experiencing a moral claim they endorse but lack enforcement power to execute. The engine derives these divergences from the authored power, exit, and role data — the claim (snare by this reading) is independent of the metrics (they describe extractiveness and suppression levels), and the divergence between claim and measured seat-types is the signal.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure hinges on exit options and power asymmetry. Refugee diaspora are trapped (trapped exit): they cannot physically return without Israeli state consent, cannot resettle permanently in host nations (most are stateless), and cannot stay in diaspora indefinitely (their children face perpetual legal disability). This trap amplifies their directionality toward the target end. Palestinian territorial remainder are identity-locked (identity_locked exit): their identity as Palestinians is constitutively bound to the claim of territorial rights and right of return; exit would mean accepting permanent reduced status or emigration, both of which dissolve the identity claim itself. Israeli state apparatus are highly mobile (arbitrage exit): they could theoretically negotiate a different territorial arrangement, recognize Palestinian sovereignty, and resettle Israeli citizens in the reduced territory — the cost is political (domestic opposition) not existential. This power asymmetry — trapped/identity-locked payers vs. arbitrage-capable beneficiaries — is the structure that sustains the constraint despite resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (Jewish refuge and safety) is contested but was live at t0 (1948). By contemporary time (t75 ≈ 2023), the founding problem status is deeply contested: Israeli sources argue Jewish safety requires permanent territorial dominance; Palestinian and international sources argue Jewish safety is separable from Palestinian dispossession and the founding problem has been solved by the existence of a Jewish state while the 'solution' of territorial seizure created a new problem (Palestinian statelessness). The disappearance verdict is world_rearranges: if this constraint disappeared (occupation ended, return honored, Israeli state negotiated borders with Palestinian state), the entire regional order would reorganize. This mismatch — founding_problem_status contested/overridden, disappearance verdict rearranges, theater_ratio rising, extractiveness accumulating — triggers mandatrophy review: the constraint persists beyond its founding justification and is now maintained primarily by enforcement machinery and demographic control rather than by solving an active problem. The classification as snare is appropriate because the constraint's persistence depends entirely on suppression and exit denial, not on solving a genuine collective-action problem for the dominated seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    return_feasibility_and_identity_fusion,
    'Is the right of return politically infeasible because it is structurally incompatible with Israeli Jewish-state legitimacy, or because the identity claim to return has become so fused with Palestinian political identity that compromise on return numbers/mechanisms is impossible even when feasible?',
    'Counterfactual: if Israeli state formally agreed to return of 50% of diaspora (feasible number to preserve Jewish demographic majority) would Palestinian leadership accept, or does the identity claim require universal return rights even if implementation is phased/voluntary? Also: post-constraint scenario observation — if occupation ended without return, would resistance persist because return remains existentially required, or would it abate as political autonomy is achieved?',
    'If return is a structural requirement of Palestinian identity, the constraint cannot be resolved without creating a new identity crisis in Palestinian politics. If return is a negotiable tactic within the identity claim, a two-state compromise becomes feasible and the constraint becomes scaffold or tangled rope rather than snare. Classification depends on whether return is intrinsic or instrumental to legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(return_feasibility_and_identity_fusion, conceptual, 'Whether right of return is a non-negotiable structural claim or a contested tactic.').

omega_variable(
    suppression_internalization_and_post_constraint_persistence,
    'If the external suppression (military occupation, permit systems, movement restrictions) were removed tomorrow, would Palestinian resistance and return assertion persist at current levels, persist at reduced levels after trauma recovery, or abate because the suppression itself was the extraction target and the underlying coordination problem (coexistence) would be resolvable?',
    'Observation from scenarios of unilateral Israeli withdrawal or dramatic policy shift (e.g., South African apartheid-to-democracy transition analogs): track diaspora assertion of return rights, territorial remainder assertions of autonomy, and organizational resistance levels across 5–10 years post-constraint-removal.',
    'If suppression is primarily structural (external barriers), removal should significantly reduce resistance as people normalize to new conditions. If suppression is partially internalized (trauma, learned helplessness, fused identity), resistance persists because the constraint has become psychological/cultural and not just legal/military. Internalized suppression suggests the constraint is a piton or deep snare, not a removable obstacle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_and_post_constraint_persistence, empirical, 'Whether Palestinian resistance is driven by current suppression or by deeper identity/trauma fusion.').

omega_variable(
    jewish_refuge_necessity_and_territorial_scope,
    'Is the founding problem (Jewish safety requiring territorial refuge) legitimately solved by the existence of a Jewish-majority state with secure borders, or does Jewish safety require the specific territory currently occupied (all of 1948 territory plus occupied lands) to be fully under Jewish state control?',
    'Historical analysis: Jewish safety and security metrics before and after 1967 occupation — did Jewish safety improve materially with occupation, or did it introduce new security threats that the occupation claims to prevent? Also: evidence from other refuges (UK Jewish population, French Jewish population, diaspora Jewish communities) on whether Jewish safety requires territorial dominance or is achievable under minority-status with legal protection.',
    'If Jewish safety is achievable without occupying Palestinian land, the founding problem is separated from the solution (occupation), and the occupation becomes pure extraction rather than necessary remedy. This would downgrade the constraint''s justification even on the zionist_refuge_reading and make this autochthony_reading''s claim harder to contest. If Jewish safety genuinely requires territorial dominance, the founding problem remains live and the constraint has residual coordination content (security) that this reading underweights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_refuge_necessity_and_territorial_scope, empirical, 'Whether occupation is necessary for Jewish safety or merely claimed to be.').

omega_variable(
    two_state_coexistence_reading_feasibility,
    'Is the two_state_coexistence_reading (sibling reading, OQ-254 kernel mapping) a genuine live alternative that honors both autochthony_reading and zionist_refuge_reading claims simultaneously, or does it foreclose this reading''s claim by making return non-universal?',
    'Logical analysis: does a two-state framework with borders at 1967 lines + negotiated swaps + international right-of-return guarantee (unlimited annual return to Palestinian state, capped return to Israeli state as part of peace agreement) satisfy both foundational axioms (Palestinian autochthony/return and Jewish refuge/security), or does capping return necessarily foreclose the autochthony_reading''s axiom of universal return right?',
    'If two-state is a genuine coexistence frame that does NOT foreclose autochthony, the relation between this reading and two_state_coexistence_reading is ''coexists_with'' (both remain live). If two-state necessarily forecloses universal return as a structural matter, the relation is ''forecloses'' (one reading''s core premise rules out the other). This affects how the committer frame understands the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_coexistence_reading_feasibility, conceptual, 'Whether two-state compromise forecloses or preserves the autochthony reading''s core claim.').

omega_variable(
    kernel_reading_vs_neutral_policy_analysis,
    'Is this constraint story a reading of a contested political kernel (one of three legitimate committer positions on territorial legitimacy), or is it a purported objective analysis of occupation dynamics that has been mistakenly formatted as a kernel reading?',
    'Verification: does the authored narrative adopt Palestinian legitimacy premises as its starting point (reading frame), or does it attempt to evaluate all three territorial claims from a neutral standpoint and then measure them? Kernel readings START from one reading''s premises; they do not start neutral and conclude at a reading. If this story was authored neutral-starting, it should not have a cs_structure block and should have three separate constraint files, one per reading, not one file claiming to be THE reading.',
    'If this is a genuine reading (starting from Palestinian autochthony premises), the ε and metrics and classification are correct as authored from that frame. If this was neutral-starting and mistakenly formatted as a reading, the classification is suspect and the story needs restructuring to either (a) drop the reading frame and write it as a neutral occupation-dynamics story, or (b) clearly author it from the Palestinian premises from sentence one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_neutral_policy_analysis, conceptual, 'Verification that this story is genuinely a kernel reading and not neutral analysis misframed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(terr_tr_t10, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(terr_tr_t25, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(terr_tr_t75, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(terr_be_t10, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(terr_be_t25, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(terr_be_t75, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 75, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(terr_su_t10, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(terr_su_t25, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 25, 0.86).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(terr_su_t75, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 75, 0.91).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=75
narrative_ontology:measurement(terr_grid_01, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(class), 0, 0.75).
narrative_ontology:measurement(terr_grid_02, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(class), 75, 0.82).
narrative_ontology:measurement(terr_grid_03, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(terr_grid_04, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(individual), 75, 0.81).
narrative_ontology:measurement(terr_grid_05, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(terr_grid_06, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(organizational), 75, 0.76).
narrative_ontology:measurement(terr_grid_07, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(structural), 0, 0.78).
narrative_ontology:measurement(terr_grid_08, territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse(structural), 75, 0.85).
narrative_ontology:measurement(terr_grid_09, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(terr_grid_10, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(class), 75, 0.78).
narrative_ontology:measurement(terr_grid_11, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(terr_grid_12, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(individual), 75, 0.64).
narrative_ontology:measurement(terr_grid_13, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(terr_grid_14, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(organizational), 75, 0.76).
narrative_ontology:measurement(terr_grid_15, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(structural), 0, 0.75).
narrative_ontology:measurement(terr_grid_16, territorial_legitimacy_dual__palestinian_autochthony_reading, resistance(structural), 75, 0.82).
narrative_ontology:measurement(terr_grid_17, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(class), 0, 0.79).
narrative_ontology:measurement(terr_grid_18, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(class), 75, 0.88).
narrative_ontology:measurement(terr_grid_19, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(individual), 0, 0.81).
narrative_ontology:measurement(terr_grid_20, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(individual), 75, 0.91).
narrative_ontology:measurement(terr_grid_21, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(organizational), 0, 0.74).
narrative_ontology:measurement(terr_grid_22, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(organizational), 75, 0.85).
narrative_ontology:measurement(terr_grid_23, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(structural), 0, 0.82).
narrative_ontology:measurement(terr_grid_24, territorial_legitimacy_dual__palestinian_autochthony_reading, stakes_inflation(structural), 75, 0.89).
narrative_ontology:measurement(terr_grid_25, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(class), 0, 0.78).
narrative_ontology:measurement(terr_grid_26, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(class), 75, 0.91).
narrative_ontology:measurement(terr_grid_27, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(individual), 0, 0.75).
narrative_ontology:measurement(terr_grid_28, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(individual), 75, 0.89).
narrative_ontology:measurement(terr_grid_29, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(organizational), 0, 0.81).
narrative_ontology:measurement(terr_grid_30, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(organizational), 75, 0.92).
narrative_ontology:measurement(terr_grid_31, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(structural), 0, 0.8).
narrative_ontology:measurement(terr_grid_32, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression(structural), 75, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (palestinian_autochthony_reading) of the territorial_legitimacy_dual kernel. The kernel represents the fundamental contested commitment to territorial legitimacy — whose historical claim justifies territorial possession. This reading grounds legitimacy in Palestinian continuous habitation, intergenerational displacement trauma, and the non-negotiable right of return. The sibling readings (zionist_refuge_reading and two_state_coexistence_reading) represent different parties' structural interpretation of the same territorial legitimacy kernel, each with different victim/beneficiary structures and different remedial claims. These are NOT the same constraint viewed from different angles — they are three structurally distinct constraints with different ε values, different beneficiary/victim sets, and different types. They are linked via network.affects_constraints to show they are members of the same constraint family and to enable contamination propagation analysis across the kernel readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

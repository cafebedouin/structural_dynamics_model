% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious-Zionist Divine Promise / Messianic Restoration Reading of Territorial Legitimacy
 *   domain: political/religious/settler_colonialism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE of three structurally distinct constraints
 *   emitted by a single contested kernel — the legitimacy basis of Zionist
 *   territorial claims. This reading holds that the 1967 conquest of the West
 *   Bank, Gaza, Sinai, and the Golan constituted a stage in an unfolding
 *   messianic-redemptive process, drawing on the theology developed by Rabbi
 *   Zvi Yehuda Kook and institutionalized by Gush Emunim and its successors.
 *   Under this reading, sovereignty over the biblical Land of Israel is a
 *   religious obligation that overrides ordinary diplomatic, security, or
 *   humanitarian calculus, and territorial maximalism (retention and
 *   settlement of the full historical land) is theologically mandated rather
 *   than one policy option among several. This is not the national-liberation
 *   reading (secular return of a persecuted people to an ancestral homeland,
 *   negotiable through ordinary politics) nor the settler-colonial reading
 *   (an ethno-national project of indigenous displacement analyzed through
 *   colonial theory) — those are separate constraints with separate ε values,
 *   separate victim/beneficiary structures, and separate stakeholder sets,
 *   linked here only through network.affects_constraints. Conflating the
 *   three into a single 'Zionism' constraint would violate the ε-invariance
 *   principle: measuring extraction through the theological lens versus the
 *   colonial-analysis lens yields very different numbers, which is exactly
 *   the signal that these are different constraints.
 *
 * KEY AGENTS:
 *   - religious_settler_movement: primary agenda-setter and identity-locked believer (organized/identity_locked) — the theology constitutes its purpose
 *   - palestinian_residents_of_settled_territories: primary payer (powerless/trapped) — bear expropriation and restriction justified by a covenant they are not party to
 *   - national_religious_political_parties: institutional beneficiary (powerful/constrained) — converts doctrine into governing leverage
 *   - non_religious_zionist_political_factions: displaced same-side actor (moderate/constrained) — loses control of what the founding national project is taken to mean
 *   - international_legal_and_historical_observers: analytical observer (analytical/analytical) — documents the doctrine's operation against international law and historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.71).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.68).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious-Zionist Divine Promise / Messianic Restoration Reading of Territorial Legitimacy").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political/religious/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'b6e08b83-cee1-4c1b-bad8-89ef49356222').
narrative_ontology:cs_kernel_codification('b6e08b83-cee1-4c1b-bad8-89ef49356222', distributed).
narrative_ontology:cs_authority_grounding('b6e08b83-cee1-4c1b-bad8-89ef49356222', lineage).
narrative_ontology:cs_interpretation_layer_present('b6e08b83-cee1-4c1b-bad8-89ef49356222').
narrative_ontology:cs_reading_relation('b6e08b83-cee1-4c1b-bad8-89ef49356222', zionist_legitimacy_basis__national_liberation_reading, influences).
narrative_ontology:cs_reading_relation('b6e08b83-cee1-4c1b-bad8-89ef49356222', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('b6e08b83-cee1-4c1b-bad8-89ef49356222', foundational, territorial_sovereignty_as_covenantal_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_as_covenantal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b6e08b83-cee1-4c1b-bad8-89ef49356222', territorial_sovereignty_as_covenantal_obligation, theological).
narrative_ontology:cs_axiom('b6e08b83-cee1-4c1b-bad8-89ef49356222', foundational, political_conquest_as_messianic_process_stage).
narrative_ontology:cs_axiom_status(political_conquest_as_messianic_process_stage, holdable).
narrative_ontology:cs_axiom_grounding('b6e08b83-cee1-4c1b-bad8-89ef49356222', political_conquest_as_messianic_process_stage, theological).
narrative_ontology:cs_axiom('b6e08b83-cee1-4c1b-bad8-89ef49356222', secondary, territorial_negotiability_permitted).
narrative_ontology:cs_axiom_status(territorial_negotiability_permitted, overridden).
narrative_ontology:cs_axiom_grounding('b6e08b83-cee1-4c1b-bad8-89ef49356222', territorial_negotiability_permitted, theological).
narrative_ontology:cs_reference_frame('b6e08b83-cee1-4c1b-bad8-89ef49356222', kookian_messianic_process_theology).
narrative_ontology:cs_drift_state('b6e08b83-cee1-4c1b-bad8-89ef49356222', post_oslo_settlement_expansion_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('b6e08b83-cee1-4c1b-bad8-89ef49356222', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, gush_emunim_successor_organizations).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, national_religious_political_parties).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, settlement_enterprise_administrators).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_residents_of_settled_territories).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_peace_constituency).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, non_religious_zionist_political_factions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, displaced_bedouin_and_agrarian_communities).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, covenantal_land_promise_doctrine).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_process_theology).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, greater_land_of_israel_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes settlement construction in the West Bank on the theological premise that Jewish sovereignty over the biblical Land of Israel constitutes an unfolding stage of messianic redemption accelerated by the 1967 territorial conquest. Sets facts on the ground, lobbies government ministries for infrastructure and legal recognition, and treats withdrawal from any settled territory as a religious transgression rather than a negotiable policy choice. Its identity is constituted by the theological reading itself, making retreat from the claim functionally equivalent to abandoning the movement's reason for existing.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement, agenda_setter,
    organized, civilizational, identity_locked, regional).

% Institutional descendants of the post-1967 settlement vanguard; run yeshivot, settlement councils, and advocacy networks that translate the messianic-process theology into concrete land acquisition, outpost authorization requests, and demographic strategy. They receive state resources, security infrastructure, and political patronage that flow specifically because the religious framing supplies a legitimacy narrative competing frameworks cannot supply.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, gush_emunim_successor_organizations, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, gush_emunim_successor_organizations, beneficiary).

% Convert the theological claim into governing coalitions, ministerial portfolios, and budget allocations for settlement infrastructure. Their electoral base is organized around the doctrine's territorial maximalism, giving them leverage disproportionate to their vote share whenever coalition arithmetic requires their support.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, national_religious_political_parties, beneficiary,
    powerful, generational, constrained, national).

% Government bodies and quasi-governmental land authorities that process authorization, zoning, and subsidy for settlement expansion. They administer under a legal architecture whose political durability rests substantially on the theological claim's ability to override ordinary land-use and international-law objections.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, settlement_enterprise_administrators, beneficiary,
    institutional, biographical, mobile, national).

% Live under a legal and administrative regime whose territorial claims are grounded, in this reading, in scriptural inheritance rather than negotiated sovereignty. Face land expropriation, movement restriction, and settlement expansion justified by a covenant to which they are not party and which explicitly treats their continued residence as an obstacle to a religious process. Have no forum in which the theological premise itself can be contested, since it sits outside the domain of ordinary political negotiation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_residents_of_settled_territories, payer,
    powerless, biographical, trapped, regional).

% Israeli citizens and political factions who favor territorial compromise for security, economic, or civic-liberal reasons but find that any withdrawal proposal is met not with a policy counter-argument but with a claim of religious inviolability that forecloses negotiation. Bear the diplomatic isolation, security costs, and internal political fracture that accompany a territorial policy set by theological rather than strategic criteria.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_peace_constituency, payer,
    moderate, biographical, constrained, national).

% Labor Zionist and liberal-nationalist currents whose own legitimacy claim (national liberation of a persecuted people) is increasingly displaced in public discourse by the religious-restoration framing, particularly after 1967 and especially after 1973 and the rise of settler politics. They lose control over what 'Zionism' is taken to mean and find their own historically dominant framing treated as insufficiently committed.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, non_religious_zionist_political_factions, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, non_religious_zionist_political_factions, excluded).

% Pastoral and agrarian communities in the Jordan Valley and South Hebron Hills whose land is designated for settlement expansion or military closure under policies whose ultimate justification traces to the territorial-maximalist theology. Have essentially no institutional access to challenge the premise driving the land designation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, displaced_bedouin_and_agrarian_communities, payer,
    powerless, biographical, trapped, local).

% A minority current within Orthodox Judaism (including some Haredi authorities) that rejects the theological claim that political sovereignty achieved by human political and military action constitutes authentic messianic fulfillment, holding that redemption cannot be forced. Their theological objection is marginalized within Israeli religious-Zionist discourse and rarely reaches policy forums.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, orthodox_religious_authorities_opposing_messianic_activism, excluded,
    moderate, generational, constrained, national).

% Scholars, UN bodies, and international courts assessing the territorial claim against international humanitarian law and historical record. They document the doctrine's operation, its policy consequences, and its relationship to the rival legitimacy readings, without holding power to alter the constraint directly.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_legal_and_historical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying, non-negotiable framework that coordinates a religious-nationalist settler movement, allied political parties, and state land administration around a single continuous project of territorial retention and expansion, resolving what would otherwise be competing tactical and strategic disagreements about where and how fast to settle.
% TRANSFER_FUNCTION: Moves land, water access, freedom of movement, and physical security from Palestinian residents and Bedouin/agrarian communities in the West Bank to the religious settler movement and the state apparatus administering settlement expansion; moves political capital and coalition leverage from broader Israeli society to national-religious parties.
% ABSENT_VOICES: Palestinian residents whose land and legal status are at stake have no standing within the theological framework, which treats their claims as outside its domain of adjudication. Orthodox authorities who reject forced messianism are marginalized within religious-Zionist institutions themselves. Secular Zionist historians and non-religious founders of the state are effectively written out of the movement's own genealogy when territorial maximalism is cast as its truest expression.
% DISAPPEARANCE_RATIONALE: If the theological legitimacy claim vanished as an operative political force, settlement expansion would lose its principal claim to inviolability, Israeli territorial policy would revert to being negotiable on security/diplomatic grounds, national-religious parties would lose their distinguishing platform, and land-use decisions in the West Bank would become contestable through ordinary political and legal channels rather than being shielded by covenantal claims.
% FOUNDING_PROBLEM: After the 1967 war's rapid territorial acquisition, religious Zionist thinkers (notably followers of Rabbi Zvi Yehuda Kook) sought to explain the conquest theologically: was this military and political event a meaningless historical accident, or a stage in divinely ordained redemption? The doctrine was built to resolve this interpretive crisis by asserting the latter, converting battlefield outcomes into religious obligation.
% FOUNDING_PROBLEM_CORROBORATION: The religious settler movement and its allied rabbinic authorities attest that the messianic process remains actively unfolding and requires continued territorial consolidation. Independent historians of religious Zionism (e.g., scholarship on Gush Emunim's theological genealogy) and international legal bodies corroborate that the doctrine continues to function as the operative justification for settlement policy, though they characterize its 'liveness' as a sustained political project rather than a metaphysical fact — a characterization the movement's own theological authorities would reject as reductive.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as substantial and rising (0.28 in 1967 to 0.71 by 2024) because the theological claim's practical effect — land transfer, movement restriction, settlement subsidy — accumulated steadily as the movement institutionalized after 1977 (Likud's rise) and again after the 1990s settlement expansion. Suppression is authored as rising in step (0.30 to 0.68) because the doctrine's persistence increasingly depends on foreclosing negotiation itself: once a claim is framed as divinely mandated, ordinary political compromise becomes a religious transgression, which requires active political and sometimes physical enforcement (outpost defense, legal shielding of settlement expansion) to sustain against both Palestinian resistance and Israeli domestic dissent. Theater ratio is kept comparatively low (0.10 to 0.22) because the coordination function — organizing settlement, political representation, and land administration — is genuinely operative, not merely performative; this is a tangled rope, not a piton. Accessibility collapse is moderate (0.42) rather than high because rival legitimacy framings (national-liberation, settler-colonial, secular-strategic) remain publicly contested and available, unlike a true mountain where alternatives have vanished. Resistance is high (0.74) reflecting sustained Palestinian, international, and secular-Israeli contestation of the doctrine's practical consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious settler movement and its institutional descendants are the structural beneficiaries: the doctrine supplies the legitimacy narrative that unlocks land, subsidy, and political power, and their identity is constituted by the claim (low d, identity-locked toward benefit). Palestinian residents and displaced agrarian communities are full targets: trapped exit options, no standing within the framework that governs them, bearing the transfer directly (high d). Secular Israeli peace constituencies and non-religious Zionist factions occupy an intermediate but genuinely costly position — they are Israeli citizens who benefit from state structures generally but pay specifically through the diplomatic, security, and internal-legitimacy costs the theological maximalism imposes on the polity as a whole; their exit is constrained rather than trapped because they retain citizenship and voice, but that voice is structurally disadvantaged against a claim that presents itself as beyond negotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to theologically interpret the unexpected 1967 territorial acquisition) is authored as status: live — the movement's own authorities maintain the redemptive process is ongoing and requires continued action. This prevents a naive mandatrophy verdict (declaring the doctrine simply obsolete) because, from inside the tradition, the problem it was built to address has not disappeared; it has arguably intensified as settlement has deepened. But the disappearance_verdict of world_rearranges combined with a live founding-problem status is itself diagnostic: a genuinely dead-but-persisting mandate would show status:dead + world_rearranges (the zombie-capture pattern); here the mismatch is absent, which is consistent with a functioning (if severely extractive) tangled rope rather than a pure inertial piton. The corroboration record shows the 'liveness' claim is asserted from inside the benefiting tradition and contested (not corroborated) by outside historians and legal bodies, who read the doctrine's persistence as a sustained political project rather than a metaphysical necessity — this gap is exactly what the corroboration field is designed to surface, not resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_versus_political_causation,
    'Does the religious-restoration doctrine independently cause territorial maximalism, or does it retroactively sanctify a strategic/security-driven expansion that would have occurred on secular grounds regardless?',
    'Comparative case analysis of settlement decisions where security/strategic rationale and theological rationale diverge — e.g., settlements built in locations with no defensible security logic but strong theological significance (Hebron) versus those with primarily strategic logic (Jordan Valley) — would help separate the doctrine''s independent causal weight from its legitimating function.',
    'If the doctrine is primarily legitimating rather than causally generative, the constraint''s coordination function is thinner than authored and its extraction is better modeled as security/nationalist extraction wearing theological cover — closer to the settler_colonial_reading''s structure. If the doctrine independently drives decisions against strategic advice, its distinct causal role as a religious-restoration constraint is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_versus_political_causation, empirical, 'Whether the theology causes or merely legitimates territorial maximalism.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three kernel readings (national_liberation, religious_restoration, settler_colonial) genuinely different constraints with different ε values, or is the appearance of difference an artifact of choosing different observables to evaluate the same underlying territorial project?',
    'Per the ε-invariance principle, this is resolved by decomposition rather than measurement: each reading is authored as a separate constraint story with its own stakeholders and metrics, linked by network.affects_constraints. The test is whether each reading''s ε is stable under its own observable set — if it is, the readings are genuinely distinct constraints, not measurement artifacts of one.',
    'Confirms the decomposition strategy used across the three sibling stories; if any single reading''s ε proved unstable under its own internal observable set, that reading itself would require further decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three-reading decomposition is structurally warranted or a modeling convenience.').

omega_variable(
    internal_theological_dissent_weight,
    'How much structural weight should be given to the minority Orthodox position that rejects human political/military action as authentic messianic fulfillment (anti-Zionist and non-Zionist ultra-Orthodox currents), given that this dissent comes from within the same broad religious tradition rather than from secular or external critics?',
    'Track institutional representation and resource allocation to anti-messianic-activism religious authorities versus pro-settlement religious authorities over the interval; a widening gap would indicate the dissenting position is being actively marginalized rather than merely being a minority view.',
    'If the dissenting position is being actively suppressed within religious institutions (not just outvoted), this raises the authored suppression metric and strengthens the case that the doctrine requires active enforcement even within its own tradition, not only against external Palestinian and secular-Israeli resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_theological_dissent_weight, empirical, 'Whether intra-religious theological dissent is suppressed or merely marginal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(zion_tr_t1977, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1977, 0.13).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(zion_tr_t1997, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1997, 0.17).
narrative_ontology:measurement(zion_tr_t2007, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2007, 0.19).
narrative_ontology:measurement(zion_tr_t2017, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2017, 0.21).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.28).
narrative_ontology:measurement(zion_be_t1977, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1987, 0.51).
narrative_ontology:measurement(zion_be_t1997, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1997, 0.58).
narrative_ontology:measurement(zion_be_t2007, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2007, 0.64).
narrative_ontology:measurement(zion_be_t2017, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2017, 0.69).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(zion_su_t1977, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1977, 0.44).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1987, 0.53).
narrative_ontology:measurement(zion_su_t1997, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1997, 0.58).
narrative_ontology:measurement(zion_su_t2007, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2007, 0.62).
narrative_ontology:measurement(zion_su_t2017, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2017, 0.66).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.08).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the zionist_legitimacy_basis kernel. national_liberation_reading treats the same historical territorial claim as a negotiable outcome of collective self-determination by a persecuted people; settler_colonial_reading treats it as a colonial displacement project. Each reading carries its own ε, beneficiary/victim structure, and classification — this story's high extraction and rising suppression reflect specifically the theological-obligation framing's practical operation post-1967, not a composite judgment across all three readings. Do not average or reconcile ε values across the family; the divergence between readings is the analytical content, not noise to be resolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

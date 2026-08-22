% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise Eretz Yisrael — Religious Zionist Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The religious Zionist reading grounds Jewish territorial sovereignty in
 *   the Torah's covenant promise of Eretz Yisrael to the Jewish people. It
 *   frames return to the land and settlement as theological obligation and
 *   messianic redemption, not merely political aspiration or strategic
 *   advantage. Under this reading, the territory is non-negotiable because it
 *   is divinely promised; Palestinian displacement is positioned as
 *   subordinate to a superior (theological) claim. The constraint carries
 *   high extractiveness (0.88) because it allocates territory from
 *   Palestinian to Jewish control while framing that allocation as beyond
 *   human negotiation — as divine decree. Suppression is high (0.76) because
 *   maintaining the constraint requires active enforcement (military
 *   occupation, settlement expansion, legal restrictions) against Palestinian
 *   resistance and against Jewish movements that contest the reading. The
 *   temporal measurements show steady intensification from 1882 (early
 *   Zionist settlement, extractiveness 0.45) through 2024, with sharpest
 *   increases at 1948 (statehood, extractiveness jumps to 0.72) and 1967
 *   (territorial occupation following the Six-Day War, extractiveness reaches
 *   0.81). This is NOT a constraint story about whether the reading is true —
 *   it describes the structural operation of the constraint under the
 *   reading's own terms and measures how extraction and enforcement have
 *   evolved.
 *
 * KEY AGENTS:
 *   - Jewish people (as covenant community under the reading): beneficiary + agenda-setter; organized power; civilizational time horizon; identity-locked exit
 *   - Israeli religious-nationalist settlers: beneficiaries; powerful institutional and organized nodes; biographical horizon; identity-locked exit
 *   - Palestinian Arabs in the claimed territory: payers; moderate organized power; generational horizon; constrained exit (military, legal, political)
 *   - Palestinian refugees and diaspora: payers; powerless; generational horizon; trapped exit
 *   - Israeli secular and liberal institutions: dual role (beneficiary via legitimation, payer via enforcement costs and international isolation); institutional power; generational horizon; constrained exit
 *   - Other Jewish movements (diasporist, post-Zionist, anti-Zionist Orthodox): excluded; organized; generational horizon; constrained exit from the reading's institutional dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.76).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise Eretz Yisrael — Religious Zionist Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '0ba05200-1a80-4641-9940-35c1262dd719').
narrative_ontology:cs_kernel_codification('0ba05200-1a80-4641-9940-35c1262dd719', fixed_text).
narrative_ontology:cs_authority_grounding('0ba05200-1a80-4641-9940-35c1262dd719', lineage).
narrative_ontology:cs_interpretation_layer_present('0ba05200-1a80-4641-9940-35c1262dd719').
narrative_ontology:cs_reading_relation('0ba05200-1a80-4641-9940-35c1262dd719', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ba05200-1a80-4641-9940-35c1262dd719', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('0ba05200-1a80-4641-9940-35c1262dd719', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('0ba05200-1a80-4641-9940-35c1262dd719', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('0ba05200-1a80-4641-9940-35c1262dd719', foundational, torah_territorial_covenant_divine_promise).
narrative_ontology:cs_axiom_status(torah_territorial_covenant_divine_promise, holdable).
narrative_ontology:cs_axiom_grounding('0ba05200-1a80-4641-9940-35c1262dd719', torah_territorial_covenant_divine_promise, theological).
narrative_ontology:cs_axiom('0ba05200-1a80-4641-9940-35c1262dd719', foundational, jewish_covenant_community_inalienable_claim).
narrative_ontology:cs_axiom_status(jewish_covenant_community_inalienable_claim, holdable).
narrative_ontology:cs_axiom_grounding('0ba05200-1a80-4641-9940-35c1262dd719', jewish_covenant_community_inalienable_claim, deontological).
narrative_ontology:cs_axiom('0ba05200-1a80-4641-9940-35c1262dd719', secondary, messianic_redemption_through_territorial_return).
narrative_ontology:cs_axiom_status(messianic_redemption_through_territorial_return, holdable).
narrative_ontology:cs_axiom_grounding('0ba05200-1a80-4641-9940-35c1262dd719', messianic_redemption_through_territorial_return, theological).
narrative_ontology:cs_reference_frame('0ba05200-1a80-4641-9940-35c1262dd719', torah_covenant_perpetual_title).
narrative_ontology:cs_drift_state('0ba05200-1a80-4641-9940-35c1262dd719', contemporary_post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ba05200-1a80-4641-9940-35c1262dd719', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs_in_claimed_territory).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, israeli_religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, israeli_secular_and_liberal_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, israeli_secular_and_liberal_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_diaspora_and_refugees).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, torah_territorial_covenant).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, messianic_redemption_through_return).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the religious Zionist reading, the Jewish people are the covenant community to whom God promised Eretz Yisrael. The constraint benefits this seat by legitimizing exclusive Jewish sovereignty and settlement as theological fulfillment rather than colonial acquisition. They are the agenda-setter in that religious-nationalist institutions define the reading, determine settlement policy, and decide who counts as the legitimate covenant community. Their situation within the constraint is one of historical obligation: return to the land is not optional but religiously mandated. Exit from this framing would require abandoning the core identity claim that binds the reading — the claim that God promised the land to the Jewish people and that this promise constitutes a legal and spiritual title superior to all others.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, agenda_setter).

% Bear the costs of territorial loss, displacement, military occupation, and legal subordination. Under the religious Zionist reading, their presence in the territory is treated as non-legitimate — the land is claimed by the Jewish covenant community via divine promise, and Palestinian claims are subordinate. They face constrained exit: militarily (occupation and settlement enforcement prevent resistance success), politically (international recognition favors the Jewish state), structurally (the reading itself declares the land non-negotiable, so even partition is religiously illegitimate). They live under occupation in some areas, have restricted property and movement rights, and experience continuous settlement expansion that systematically converts Palestinian land to Jewish ownership.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs_in_claimed_territory, payer,
    moderate, generational, constrained, regional).

% Receive legitimation for settlement and territorial expansion under the claim of divine right and messianic redemption. Their identity as pioneering fulfilllers of the covenant promise is constituted through the reading itself: they see settlement not as acquisition but as fulfillment of religious obligation. Their time horizon is biographical but embedded in a civilizational narrative: their children will inherit the settled land, and the messianic process continues through future generations. Exit from settlement and withdrawal from territory would dissolve the theological meaning of their action and identity — they would be reframed from covenant-fulfilllers to occupiers.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_religious_zionist_settlers, beneficiary,
    powerful, biographical, identity_locked, regional).

% Benefit from the constraint's legitimating power: the divine-promise framework provides non-instrumental justification for territorial claims and settlement that extends beyond strategic or demographic arguments. This legitimacy reduces international pressure and domestic dissent. They also pay by inheriting the constraint's costs: international delegitimation, regional instability, internal conflict with Jewish movements that contest the reading, and the enforcement machinery (military, legal, institutional) required to maintain territorial claims and suppress alternatives. Exit is constrained because abandoning the constraint would require reconstructing the state's founding narrative and territorial basis.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_secular_and_liberal_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, israeli_secular_and_liberal_institutions, payer).

% Carry the costs of displacement, legal statelessness, and refugee status. The constraint's operation makes their historical displacement appear justified (the land is claimed by a people with superior divine title) and makes their return appear illegitimate (it would violate the covenant claim). They are trapped because they have no military power to reclaim territory, no state authority to negotiate return, and no international legal standing to override the Jewish state's claim. They are the structural victims whose presence is most thoroughly erased by the reading's logic: not subordinate, but absent from the legitimate claims calculus entirely.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_diaspora_and_refugees, payer,
    powerless, generational, trapped, global).

% Jewish movements that contest or reject the religious Zionist reading (diasporist movements, post-Zionist Israelis, anti-Zionist Orthodox communities, humanistic and secular Jewish movements) are excluded from the core beneficiary claim. They possess Jewish identity but reject the reading's core premise that divine promise produces inalienable territorial claim. Their exclusion is structural: the reading treats dissenting Jewish voices as having abandoned the covenant claim itself, thus as having abandoned Jewish authenticity. Their exit from the reading is institutionally costly (community pressure, loss of institutional support) but theoretically possible (unlike Palestinian exit, which is identity-dissolved; Jewish dissidents remain Jewish).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, other_jewish_readings_and_movements, excluded,
    organized, generational, constrained, global).

% Observe the constraint as a case where a theological claim to territory is mobilized to override modern international law's presumption that population displacement requires consent and that territorial claims rest on state authority, not divine title. The UN Conventions on human rights, the Geneva Conventions on occupation and treatment of civilians, and the International Court of Justice all articulate frameworks that conflict with the reading's claim that theological title supersedes consent-based international law. International humanitarian law authorities document violations of occupation law (settlement expansion, demolition of Palestinian structures, restrictions on movement) but lack enforcement power against a state that claims divine legitimacy.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_humanitarian_law_authorities, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish identity, historical memory, and spiritual practice around a narrative of return and sovereignty. The reading solves the problem of Jewish statelessness and diaspora vulnerability by providing a theological framework that makes return to ancestral land not merely legitimate but religiously obligatory. It coordinates diaspora Jewish communities around a common messianic narrative and provides institutional meaning for aliyah (return to land) as spiritual fulfillment rather than mere immigration. It also coordinates settlement policy, land acquisition, and institutional development around the unified claim that all land within Eretz Yisrael is held in trust for the Jewish people via the covenant.
% TRANSFER_FUNCTION: Transfers territorial sovereignty and the legitimacy of settlement from the previous occupants (Palestinians) to the Jewish people, justified by claim of divine title rather than negotiated transfer or international legal process. It also transfers the authority to define Jewish identity and legitimate Jewish practice from religious authorities in diaspora to those who control the land and carry out settlement. The transfer moves land itself — physical territory — from Palestinian to Jewish control through settlement, land purchase (often from Jewish charitable organizations), military occupation, and legal restrictions on Palestinian property ownership and use.
% ABSENT_VOICES: Palestinian voices are not absent from the constraint's operation but are systematically subordinated and declared non-legitimate within the reading's own framework. What IS structurally absent: international humanitarian-law authorities (who would argue for population consent and protection of civilian rights); other Jewish theological traditions that reject territorial maximalism (Diaspora Judaism, anti-Zionist Orthodox communities); secular Zionist voices that would rest the claim on self-determination rather than divine right; post-colonial scholars who would read the constraint as instantiating a European settlement pattern; liberal nationalist Zionists who would accept negotiated partition; and Palestinian intellectual and political leadership with voice in Israeli institutional spaces where the constraint's rules are set. The exclusion is not merely geographic but epistemic: the reading itself declares certain perspectives (Palestinian self-determination, Jewish diasporist alternatives, secular-nationalist grounds) religiously illegitimate before they are even heard.
% DISAPPEARANCE_RATIONALE: If the divine-promise framework vanished — if the theological claim to Eretz Yisrael were abandoned by Israeli institutions and religious-nationalist movements — the territorial claims would revert to liberal-nationalist grounds (self-determination rights) or strategic/security grounds, both of which permit negotiated partition. Israeli boundaries would contract to the internationally recognized 1967 borders or smaller. Palestinian displacement would be reframed as a historical injustice requiring remedy (right of return or compensation) rather than as a lesser claim superseded by divine title. The entire settlement enterprise in occupied territories would lose its theological justification and be exposed to legal liability under international humanitarian law. The diaspora-return narrative would shift from messianic obligation to historical aspiration and personal choice. Israeli state institutions would reconstruct their founding legitimacy on democratic self-determination rather than theological covenant. The world does not merely rearrange — the theological meaning structures of Jewish identity in relation to land would be remade, and with them, the institutional justification for territorial claims and Palestinian subordination.
% FOUNDING_PROBLEM: Jewish people suffered centuries of statelessness, diaspora dispersion, persecution, and material vulnerability. The religious Zionist reading solves this existential problem by reinstating the Torah's promise of return as a binding obligation on the Jewish people and on history itself — making return to Eretz Yisrael the mechanism of messianic redemption and Jewish survival. The founding problem is existential: how to restore Jewish sovereignty, end the condition of exile, and secure Jewish existence through return to the ancestral land.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist institutions and Torah scholars attest the founding problem (statelessness and exile) is live and that return is the only permanent solution. They argue that 1948 statehood was a first step but that the founding problem persists because Jewish sovereignty is incomplete without control of all Eretz Yisrael and because the diaspora condition is not yet ended. Israeli secular nationalists attest that the problem was substantially solved with 1948 statehood — the Jewish state now exists, has military power, and has international recognition. They argue that the constraint now persists not to solve the founding problem but to serve territorial maximalism and religious ideology. Palestinian scholarship, international legal authorities, and post-Zionist Israeli historians attest that the founding problem (Jewish vulnerability) has been addressed through modern state recognition, international law, and regional peace efforts; that the constraint now creates a NEW and more severe problem (Palestinian displacement and occupation); and that the theological language masks political choices that could be made differently. No consensus exists between the beneficiary and observer seats.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) because the constraint transfers territorial control from Palestinians to Jews while framing the transfer as non-negotiable — beyond the domain of human deliberation or compromise. The theological framing is the mechanism of extraction: it elevates one party's claim above all others and removes it from the calculus of competing rights. The temporal measurement series shows steady accumulation: extractiveness grew from 0.45 (early Aliyah, limited Palestinian displacement) to 0.88 (contemporary territorial claims including occupied territories, settlements, and refusal of right-of-return). This is not random drift; it tracks the constraint's operational intensification — more territory claimed, more settlement infrastructure built, more enforcement machinery deployed, more absolute the rejection of alternative territorial arrangements. Suppression is also high (0.76) because the constraint requires continuous active enforcement: military occupation in Palestinian territories, legal restrictions on Palestinian movement and property ownership, settlement expansion that systematically converts Palestinian land to Jewish ownership, and institutional pressure that marginalizes Jewish dissenting voices. The measurement shows suppression rising from 0.35 (voluntary early settlement phase) to 0.76 (contemporary occupation state). Theater ratio is moderate (0.42) — lower than piton-range but substantial — because while genuine theological practice and community coordination occur, a growing portion of the constraint's operation is performative: settlement expansion justified as security but distributed to maximize territorial claims; security measures justified as counter-terrorism but applied to enforce territorial control; theological language deployed to cover political choice that could be made differently.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Jewish people under the reading) and the payer seats (Palestinians, Palestinian diaspora) compute dramatically different constraint types from the same structural facts. From the beneficiary perspective: the constraint is a rope coordinating Jewish identity, religious practice, and historical return — it solves the problem of statelessness and diaspora vulnerability, and it provides institutional meaning for community action. The beneficiaries see extraction (0.88) as a mischaracterization; they see what they experience as coordination cost (creating and maintaining institutions), subsidy to themselves (the land and state), and obligation (the covenant binding Jewish people to return). From the payer perspective: the same constraint is a snare — territory that was theirs is claimed by a people invoking a medieval theological text that they were never party to; resistance is met with military force; alternatives (bi-national state, refugee return, partition) are declared religiously illegitimate before they are even negotiated; the theological language is the primary mechanism of suppression because it removes the question from the domain where Palestinian voice matters. The engine computes these per-seat differences from directionality: beneficiaries get low d (d near 0.0 → low/negative effective extraction), payers get high d (d near 1.0 → amplified effective extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (jewish_people_as_covenant_community) is declared as such because under the reading itself, the constraint benefits Jewish sovereignty, identity, and religious fulfillment. Directionality is very low (near 0.0 for this seat) because the constraint subsidizes Jewish institutional and territorial interests. The payer (palestinian_arabs_in_claimed_territory) bears costs: territorial loss, military occupation, legal subordination, displacement of family members. Directionality is very high (near 1.0) because the constraint extracts from them with no claimed reciprocal benefit. Israeli secular and liberal institutions are dual-positioned: they benefit from the constraint's legitimating power (it provides theological cover for claims they might struggle to defend on nationalist or strategic grounds alone) but they also pay by inheriting the constraint's international delegitimation and enforcement costs. Their directionality is moderate (near 0.5) — they are neither pure beneficiaries nor pure targets. The overarching asymmetry: one party's theological identity claim is treated as a superior lien on territory compared to another party's residential, agricultural, and political presence. This is not overcome by invoking both parties' attachment to the land — the reading structurally elevates one attachment above the other via the theological framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of mandatrophy (the founding problem has atrophied while the constraint persists). The founding problem was statelessness and diaspora vulnerability: Jewish people lacked political sovereignty and faced persecution across the globe. With Israeli statehood (1948), the founding problem was substantially solved: Jews have a state, they have military power, they have legal protection and international recognition. Yet the constraint has intensified rather than relaxed — territory claimed grows, settlements expand, enforcement machinery hardens. The reading redefines the founding problem as messianic redemption (the theological obligation to return to all of Eretz Yisrael), which is by nature without endpoint: you cannot solve messianic redemption through territorial acquisition because the covenant claim is infinite. This redefinition allows the constraint to persist and intensify even after its original function (solving statelessness) is accomplished. The constraint thus exhibits mandatrophy: the primary function (Jewish sovereignty) was achieved; the secondary function (theological maximalism, settlement expansion, territorial control) has become the constraint's primary operation. Theater ratio (0.42) captures this: security justifications and theological language provide institutional theater for what has become territorial maximalism. The founding_problem_status is 'contested' because Israeli secular Zionists argue the founding problem was solved with statehood and that continued territorial expansion serves conquest, not survival; while religious Zionists argue the founding problem remains unsolved (messianic redemption is incomplete) and territorial maximalism is its solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_claim_vs_territorial_maximalism,
    'Does the divine promise of Eretz Yisrael in Torah necessitate the entire territory claimed by religious Zionists, or is the theological claim separable from the political boundary claim?',
    'Textual-historical analysis of medieval and early-modern Rabbinic jurisprudence on territorial boundaries; comparison with non-maximalist religious Zionist interpretations that accept partition as consistent with Torah covenant.',
    'If separable, the theological benefit (Jewish sovereignty + religious legitimacy for return) could be decoupled from territorial maximalism, reducing measured extractiveness; if inseparable, the constraint necessarily forecloses Palestinian self-determination in the same territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_claim_vs_territorial_maximalism, conceptual, 'Whether the theological claim entails territorial maximalism or permits negotiated boundaries.').

omega_variable(
    reading_logical_compatibility,
    'Is the religious Zionist reading''s core premise (divine promise → inalienable territorial right) logically incompatible with the liberal nationalist reading''s core premise (self-determination right → negotiated statehood)?',
    'Analysis of whether a single state could simultaneously satisfy both readings: one requires theological validation, the other requires democratic proceduralism and partition consent. Document whether the readings foreclose each other or coexist as competing framings.',
    'If they foreclose (forecloses relation): no single framework can hold both, and the engine computes whether this reading has logically displaced the siblings. If they coexist (coexists_with relation): both remain live in the same political space, indicating permanent interpretive contest rather than settled closure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_compatibility, conceptual, 'Logical relationship between religious Zionist and liberal nationalist readings of the same kernel.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.76) primarily structural (military occupation, legal restrictions, settlement enforcement) or internalized (Palestinian acceptance of subordination)?',
    'Post-constraint scenarios: if suppression were removed, would resistance intensify or would acceptance persist? Palestinian intellectual and cultural analysis of internalized versus external coercion.',
    'If primarily structural: removing enforcement would collapse the constraint. If substantially internalized: the constraint has become self-perpetuating, indicating a more stable and pernicious form of extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression in constraint operation.').

omega_variable(
    jewish_identity_lock_constitutive_vs_institutional,
    'Is the exit option ''identity_locked'' for Jewish actors because the reading constitutes Jewish identity itself, or because institutional pressure makes exit costly but theoretically possible?',
    'Historical analysis of Jewish voices that have exited the religious Zionist reading without leaving Judaism: what identity-reconstruction did they perform, and how costly was it?',
    'If constitutive: the reading is structurally self-perpetuating within the community. If institutional: exit is costly but possible, and the constraint is less inevitably self-perpetuating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_identity_lock_constitutive_vs_institutional, empirical, 'Whether identity-lock is constitutive of Jewish identity or institutional enforcement.').

omega_variable(
    reading_kernel_commission_frame,
    'This constraint is ONE reading of a contested kernel. How are the sibling readings structurally related to THIS reading via the cs_structure.reading_relations types (forecloses, coexists_with, influences)?',
    'Comparative structural analysis: (1) forecloses = core premises logically rule each other out in any single framework; (2) coexists_with = both remain live, held by different parties simultaneously; (3) influences = this reading creates downstream pressure without foreclosing.',
    'Determines whether the readings are logically incompatible or compete in a multipolar interpretive space. Foreclosing relations indicate one reading can displace others; coexisting relations indicate permanent contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_commission_frame, conceptual, 'Structural relationships between the religious Zionist reading and its siblings (liberal nationalist, settler colonial, cultural Zionist, post-Zionist).').

omega_variable(
    mandatrophy_founding_problem_attenuation,
    'Has the founding problem (Jewish statelessness and diaspora vulnerability) been solved by Israeli statehood, such that the constraint''s continued intensification (territorial expansion, settlement growth) represents mandatrophy rather than problem-solving?',
    'Comparison of the founding problem as stated in early Zionist texts (1882–1948) with contemporary religious Zionist assertions. If the founding problem has shifted from ''secure Jewish sovereignty'' to ''messianic redemption and territorial maximalism,'' the constraint exhibits mandatrophy.',
    'If mandatrophy confirmed: the constraint persists not because it solves the original problem but because the problem has been redefined to be insolvable (messianic goals have no endpoint). If the founding problem remains live (Jewish vulnerability is not solved by statehood): the constraint continues to solve a real problem and mandatrophy is not confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_founding_problem_attenuation, conceptual, 'Whether founding problem has atrophied and been replaced by an insolvable theological goal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1882, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1882, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1882, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t1882, projected).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1987, 0.39).
narrative_ontology:measurement_basis(jewi_tr_t1987, observed).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2005, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t2005, observed).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1882, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1882, 0.45).
narrative_ontology:measurement_basis(jewi_be_t1882, projected).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.81).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1987, 0.84).
narrative_ontology:measurement_basis(jewi_be_t1987, observed).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2005, 0.86).
narrative_ontology:measurement_basis(jewi_be_t2005, observed).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.88).
narrative_ontology:measurement_basis(jewi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1882, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1882, 0.35).
narrative_ontology:measurement_basis(jewi_su_t1882, projected).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.68).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1987, 0.72).
narrative_ontology:measurement_basis(jewi_su_t1987, observed).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement_basis(jewi_su_t2005, observed).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.76).
narrative_ontology:measurement_basis(jewi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__religious_zionist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_right_of_return_claim).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, two_state_solution_framework).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, international_humanitarian_law_occupation).

% DUAL FORMULATION NOTE:
% This constraint is ONE instantiation of the contested jewish_sovereignty_palestine kernel. The kernel is a single persisting commitment (the connection between Jewish people and Eretz Yisrael) that different communities read differently. The religious Zionist reading (THIS story) interprets the commitment as grounding an inalienable territorial claim through divine promise. Sibling readings decompose the same kernel into structurally distinct constraints: liberal nationalist reading grounds statehood in self-determination rights (permitting negotiated partition); settler colonial reading reframes the entire Zionist project as a European displacement pattern; cultural Zionist reading emphasizes spiritual and cultural renewal without territorial maximalism; post-Zionist reading acknowledges statehood but argues the founding narrative now obstructs equality and peace. Each reading instantiates different ε, beneficiaries, victims, and enforcement logic. All five stories are linked via network.affects_constraints to signal the kernel family. The religious Zionist reading presented here is authored from WITHIN that reading's framework (Rule 1: one reading as a clean ε-invariant constraint), not as a neutral description of the contest. The ε referent is the standing arrangement under this reading's assessment (very high extraction and suppression because the reading frames territory as non-negotiable and divinely promised). Other readings would author different ε values for the same territorial arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__religious_zionist_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

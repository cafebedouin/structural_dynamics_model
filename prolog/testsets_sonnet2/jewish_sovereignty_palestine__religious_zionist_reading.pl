% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Divine Land Grant Reading of Jewish Sovereignty Over Eretz Yisrael
 *   domain: political philosophy/religious nationalism/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading among five of the contested kernel
 *   jewish_sovereignty_palestine: the religious-Zionist reading, in which
 *   sovereignty over Eretz Yisrael derives from a divine covenant that
 *   renders the land inalienable and treats statehood as theological
 *   fulfillment rather than political achievement. Within this reading's own
 *   terms, the beneficiary is the Jewish people as covenant community;
 *   Palestinian residents of the contested territories are structurally
 *   absent from the doctrine's calculus of legitimate claimants, appearing
 *   only as a demographic or administrative fact to be managed, not as
 *   holders of a competing title requiring recognition. This is a distinct,
 *   ε-invariant constraint from its siblings — the
 *   liberal_nationalist_reading (self-determination-grounded,
 *   partition-compatible), settler_colonial_reading (structural displacement
 *   regardless of intent), cultural_zionist_reading (spiritual center,
 *   sovereignty optional), and post_zionist_reading (achieved statehood whose
 *   founding framework now obstructs civic equality) — each of which has its
 *   own ε, beneficiary/victim structure, and classification, authored in
 *   separate files and linked via network.affects_constraints. Extractiveness
 *   is authored very high here because, by this reading's own lights, no
 *   partition or territorial concession has legitimacy; the claim to land is
 *   total, not negotiable at the margin, which is precisely what generates
 *   the story's high ε relative to the liberal-nationalist sibling.
 *
 * KEY AGENTS:
 *   - covenant_community_jewish_settlers: primary beneficiary and on-the-ground agenda-setter (organized/identity_locked) — settlement as religious obligation
 *   - religious_zionist_political_parties: institutional agenda-setter (institutional/constrained) — translates theology into state policy and law
 *   - palestinian_residents_of_contested_territories: primary target (powerless/trapped) — bear expropriation and administrative subordination with no standing inside the doctrine
 *   - non_orthodox_jewish_israelis_seeking_territorial_compromise: secondary payer (moderate/constrained) — political space for compromise foreclosed by coalition dynamics
 *   - international_legal_institutions: excluded authority (institutional/analytical) — judgments noted, not recognized as competent
 *   - diaspora_jewish_observers: analytical/mobile observer, divided in sympathy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.87).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.8).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Land Grant Reading of Jewish Sovereignty Over Eretz Yisrael").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political philosophy/religious nationalism/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '2ca18b57-e480-42ed-8ceb-c92f9686dd25').
narrative_ontology:cs_kernel_codification('2ca18b57-e480-42ed-8ceb-c92f9686dd25', fixed_text).
narrative_ontology:cs_authority_grounding('2ca18b57-e480-42ed-8ceb-c92f9686dd25', lineage).
narrative_ontology:cs_interpretation_layer_present('2ca18b57-e480-42ed-8ceb-c92f9686dd25').
narrative_ontology:cs_reading_relation('2ca18b57-e480-42ed-8ceb-c92f9686dd25', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('2ca18b57-e480-42ed-8ceb-c92f9686dd25', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ca18b57-e480-42ed-8ceb-c92f9686dd25', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('2ca18b57-e480-42ed-8ceb-c92f9686dd25', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('2ca18b57-e480-42ed-8ceb-c92f9686dd25', foundational, divine_covenant_grounds_inalienable_title).
narrative_ontology:cs_axiom_status(divine_covenant_grounds_inalienable_title, holdable).
narrative_ontology:cs_axiom_grounding('2ca18b57-e480-42ed-8ceb-c92f9686dd25', divine_covenant_grounds_inalienable_title, theological).
narrative_ontology:cs_axiom('2ca18b57-e480-42ed-8ceb-c92f9686dd25', foundational, territorial_partition_is_theologically_illegitimate).
narrative_ontology:cs_axiom_status(territorial_partition_is_theologically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2ca18b57-e480-42ed-8ceb-c92f9686dd25', territorial_partition_is_theologically_illegitimate, theological).
narrative_ontology:cs_reference_frame('2ca18b57-e480-42ed-8ceb-c92f9686dd25', biblical_patriarchal_land_grant).
narrative_ontology:cs_drift_state('2ca18b57-e480-42ed-8ceb-c92f9686dd25', post_1993_oslo_peace_process_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2ca18b57-e480-42ed-8ceb-c92f9686dd25', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_political_parties).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_of_contested_territories).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, non_orthodox_jewish_israelis_seeking_territorial_compromise).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, divine_land_grant_doctrine).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, theological_fulfillment_of_statehood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Settle and hold territory understood as divinely deeded, treating presence on the land as religious obligation rather than political choice. Organize politically and materially to expand and entrench settlement, framing withdrawal from any part of the land as theological transgression rather than policy option. Their identity as covenant-bearers is constituted by continued territorial presence, foreclosing exit from the claim itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_settlers, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_settlers, agenda_setter).

% Translate the theological claim into state policy: settlement subsidies, annexation legislation, and resistance to any negotiated territorial concession. Hold coalition leverage in Israeli governments disproportionate to vote share, using it to entrench the doctrine in law and administration. Could moderate the claim but doing so would dissolve the party's founding rationale.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_political_parties, agenda_setter,
    institutional, civilizational, constrained, national).

% Live under military administration, settlement expansion, and land expropriation justified by a theological title they do not hold and cannot contest within the framework that grants it. Their residence, movement, and land tenure are subordinated to a claim that treats their presence as, at most, tolerable and at minimum obstructive to fulfillment. No recognized voice within the doctrine's own terms; exit from the territory is not available and exit from the claim's authority is not offered.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_of_contested_territories, payer,
    powerless, generational, trapped, local).

% Favor negotiated withdrawal from some territories for security or civic reasons but find the theological claim treated as non-negotiable within religious-Zionist coalition politics, constraining the space of electable policy. Bear costs in the form of prolonged conflict, international isolation, and internal political fracture, without being able to exit the national community whose politics the doctrine partially captures.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, non_orthodox_jewish_israelis_seeking_territorial_compromise, payer,
    moderate, biographical, constrained, national).

% Assert that the territories are occupied under international law and that settlement activity is unlawful; this determination carries no force within the religious-Zionist framework, which does not recognize secular international law as an authority competent to adjudicate a divine grant. Their judgments are noted and disregarded rather than engaged on the doctrine's own terms.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_legal_institutions, excluded,
    institutional, generational, analytical, global).

% Watch the doctrine's political consequences from outside the territory, divided between religious sympathy for the theological claim and discomfort with its costs to Palestinians and to Israel's international standing. Can withdraw support, funding, or advocacy without bearing direct consequences themselves.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, diaspora_jewish_observers, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the covenant community with a unifying, non-negotiable rationale for settlement and political mobilization, coordinating otherwise dispersed religious and nationalist actors around a single theological claim to specific land.
% TRANSFER_FUNCTION: Moves land, water, and administrative control from Palestinian residents to Jewish settlement enterprises and the Israeli state apparatus that enables them, using theological title as the legitimating instrument for expropriation and permit denial.
% ABSENT_VOICES: Palestinian residents of the claimed territories have no standing within the doctrine's own framework to contest the divine grant; their prior and continuous presence on the land is treated as incidental or as an obstacle to fulfillment rather than as a competing claim requiring adjudication. International legal bodies are heard but structurally disregarded as lacking authority over a theological matter.
% DISAPPEARANCE_RATIONALE: If the theological non-negotiability claim were withdrawn, settlement expansion would lose its primary domestic political justification, territorial compromise would become electorally available within religious-Zionist constituencies, and the legal and administrative architecture built to protect settlement (land seizure orders, outpost legalization, annexation bills) would lose its stated rationale, though not necessarily its political momentum from secular nationalist sources.
% FOUNDING_PROBLEM: Post-1967 and especially post-1973, a current within religious Zionism sought to resolve the tension between messianic theology (which had historically counseled passive waiting for divine redemption) and the fact of Jewish sovereignty over biblical heartland territories, by reinterpreting the state and its territorial control as themselves the beginning of messianic fulfillment requiring active human settlement.
% FOUNDING_PROBLEM_CORROBORATION: Religious-Zionist rabbinic authorities and settlement organizations affirm the founding problem as ongoing and unresolved (redemption is incomplete without full territorial consolidation). Secular Israeli security establishment figures, Israeli High Court rulings on settlement legality, and Palestinian testimony before international bodies attest from outside the covenant-community framework that the arrangement's operative function is land acquisition and demographic entrenchment rather than theological necessity, and that this function persists independent of the theological premise's truth.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.87, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.87 and rising over the interval because the doctrine's territorial maximalism (no legitimate partition) combines with escalating settlement infrastructure, land seizure mechanisms, and permit regimes that transfer land and water control from Palestinian residents to the settlement enterprise. Suppression is authored high (0.80) and rising because maintaining the claim against contrary territorial facts (prior Palestinian residence, international legal consensus, periodic Israeli governmental willingness to negotiate) requires continuous active enforcement — military administration, outpost legalization, demolition orders, and political suppression of compromise-oriented factions within Israeli politics itself. Theater ratio is comparatively low (0.25) because the doctrine translates directly into material settlement and legal action rather than remaining primarily symbolic, though a growing performative component (declaratory annexation votes, symbolic Knesset legislation) is captured in its modest rise.
 *
 * PERSPECTIVAL GAP:
 *   From the covenant community's own seat, this is not extraction but restoration — fulfillment of a promise long deferred, in which claimed suffering (of Palestinians displaced or restricted) is a regrettable side effect of a theologically necessary process, not the arrangement's function. From the Palestinian payer seat, the same territorial administration is straightforwardly a transfer regime with theological cover. The engine computes both per-seat readings from the same structural data; this story does not adjudicate between them — it authors the religious-Zionist reading's own ε and ontology as the reading's structural fact, per the ε-referent rule for kernel readings (the referent is the standing arrangement under contest, assessed by this reading's own lights, not the reading's endorsed alternative or the sibling readings' assessments).
 *
 * DIRECTIONALITY LOGIC:
 *   The covenant community and religious-Zionist parties sit at the beneficiary/agenda-setter end: they collect land, political power, and theological vindication from the arrangement's operation, and their exit options are identity-locked or institutionally entrenched respectively — leaving the claim would mean abandoning constitutive religious or political identity. Palestinian residents sit at the full-target end: trapped, powerless within the doctrine's own terms, bearing the transfer of land and autonomy with no recognized voice. Non-orthodox Israelis seeking compromise are a secondary payer — moderate power, constrained exit — paying in foreclosed political options and prolonged conflict costs without being expropriated directly. International legal institutions are excluded rather than positioned on the payer/beneficiary axis at all; their exclusion from the doctrine's competence is precisely the point the doctrine requires to preserve its exclusivity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (theological anxiety over the gap between messianic doctrine and post-1967 sovereignty) is authored as live from within the tradition — settlement is understood as ongoing, unfinished redemptive work, not a completed and now-obsolete mandate. This forecloses treating the doctrine as a piton (function atrophied, persists by inertia): the doctrine's proponents experience it as fully functional and urgent, and material settlement activity (rising extractiveness, rising suppression) corroborates that the function is actively operating, not vestigial. Whether the arrangement should nonetheless be read as tangled_rope (coordination for the covenant community, genuine extraction from Palestinians, both riding the same territorial-control mechanism) or as a pure snare from outside this reading's own terms is exactly the divergence the kernel decomposition is designed to expose — this file authors the tangled_rope claim as descriptively true from the reading's structural operation (it does coordinate settler political mobilization) while the extraction is severe enough that a sibling reading (settler_colonial_reading) would likely author it as pure snare instead. That divergence across readings, not within this one story, is where the corpus's analytical work lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_claim_versus_political_instrumentalization,
    'Is the divine-promise doctrine a genuine, sincerely held theological commitment that independently generates settlement behavior, or is it substantially a legitimating narrative deployed over a politically and materially motivated settlement project?',
    'Comparative analysis of settlement patterns against strategic/security value versus purely theological significance (e.g., settlement density in areas of high biblical significance but low strategic value versus areas of high strategic value but low biblical significance); examination of religious-Zionist leadership statements across contexts (internal theological discourse versus international diplomatic framing).',
    'If largely sincere theological commitment, the coordination function (uniting a covenant community around shared meaning) is more genuinely present alongside the extraction, supporting a tangled_rope reading. If substantially instrumentalized, the theological framing functions primarily as cover for a land-acquisition project, pushing the classification toward pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_claim_versus_political_instrumentalization, conceptual, 'Whether the doctrine is sincere theological commitment or a legitimating narrative over material extraction.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the religious_zionist_reading structurally diverge from its sibling readings, and does that divergence admit of any adjudication from outside all five readings simultaneously?',
    'This is the committer-structure omega required by Rule 2: the disagreement is located at the beneficiary/victim calculus (does the doctrine recognize Palestinian claimants as parties at all?) and at the negotiability of the land itself (is any partition legitimate in principle?). The liberal_nationalist_reading would recognize partition as legitimate in principle even if practically difficult; the settler_colonial_reading would treat any Jewish sovereignty claim, negotiated or not, as instantiating displacement; the religious_zionist_reading uniquely forecloses partition as theologically illegitimate regardless of practical or diplomatic considerations.',
    'A sibling reading adopting a different resolution of the negotiability question would produce a different ε (much lower for liberal_nationalist, differently structured but comparably high for settler_colonial) and a different beneficiary/victim calculus (post_zionist_reading would add Israeli Jewish citizens seeking civic equality as a victim class alongside Palestinians).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committer-structure omega: naming where this reading''s premises diverge from the four sibling readings of the same kernel.').

omega_variable(
    corroboration_asymmetry,
    'Can the founding_problem_status (live) be corroborated by any source that is not either a beneficiary of the doctrine (settler/party) or a direct opponent (Palestinian testimony, international bodies) — is there a genuinely neutral corroborating seat?',
    'Survey of Israeli secular historiography and archival military/administrative records documenting settlement planning rationale (security versus theological framing in internal government deliberations, e.g. Allon Plan versus Gush Emunim settlement drives).',
    'If internal Israeli state records show settlement decisions driven primarily by security/demographic planning with theological framing added post hoc for domestic political mobilization, this supports the instrumentalization omega above and weakens the doctrine''s claim to being a sincere, non-negotiable theological necessity rather than a political program with religious branding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_asymmetry, empirical, 'Whether any source outside both beneficiary and opponent camps corroborates the founding problem''s liveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1977, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(jewi_tr_t1990, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(jewi_tr_t2012, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2012, 0.23).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(jewi_be_t1977, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1977, 0.65).
narrative_ontology:measurement(jewi_be_t1990, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(jewi_be_t2012, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2012, 0.83).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(jewi_su_t1977, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1977, 0.55).
narrative_ontology:measurement(jewi_su_t1990, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(jewi_su_t2012, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2012, 0.75).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__religious_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraints decomposed from the single natural-language label 'the Zionist claim to sovereignty over Eretz Yisrael / Palestine,' per the ε-invariance principle: the label conflates structurally distinct claims (theological non-negotiability, secular self-determination, colonial-pattern displacement, cultural-spiritual project, post-sovereignty civic critique) that carry different ε, different beneficiary/victim structures, and different classifications. Each sibling is authored as its own file with its own claimed_type; this file's religious_zionist_reading is distinguished from liberal_nationalist_reading by foreclosing partition as illegitimate in principle (not merely impractical), and from settler_colonial_reading by locating the extraction's legitimating mechanism in divine title rather than in a colonial-pattern analysis that would apply regardless of the settlers' self-understanding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__religious_zionist_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

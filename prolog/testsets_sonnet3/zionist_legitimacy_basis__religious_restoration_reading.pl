% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Religious-Zionist Reading: Territorial Settlement as Divine Covenant Fulfillment
 *   domain: political/religious/territorial
 *
 * SUMMARY:
 *   This story instantiates one reading among three of the contested 'Zionist
 *   legitimacy' kernel: the post-1967 religious-Zionist interpretation that
 *   reads the Six-Day War's territorial conquest as an unfolding stage of
 *   divine redemption (the Gush Emunim tradition and its rabbinic successors,
 *   grounded in Rav Kook's theology as elaborated by Rav Tzvi Yehuda Kook).
 *   This reading is generated as its own ε-invariant constraint: it does not
 *   average with, hedge against, or describe the national-liberation reading
 *   or the settler-colonial reading, which are separate constraints in this
 *   family with their own beneficiary/victim structures and their own ε. The
 *   distinguishing structural claim here is that territorial retention is a
 *   religious obligation that precedes and outranks ordinary political
 *   negotiation — a claim the other two readings do not make in this form.
 *
 * KEY AGENTS:
 *   - religious_settler_movement: primary agenda-setter and beneficiary, organized/identity_locked
 *   - religious_zionist_rabbinic_authorities: interpretive authority that transmits and enforces the doctrine, institutional/identity_locked
 *   - west_bank_palestinian_residents: primary payers, powerless/trapped
 *   - secular_israeli_land_for_peace_constituency: secondary payers, moderate/constrained — foreclosed from a negotiating position
 *   - international_legal_observers: excluded analytical seat, treated as categorically inapplicable within the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.68).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.72).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious-Zionist Reading: Territorial Settlement as Divine Covenant Fulfillment").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political/religious/territorial").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'c536c2d1-2a45-434b-b6bb-6e40a4095491').
narrative_ontology:cs_kernel_codification('c536c2d1-2a45-434b-b6bb-6e40a4095491', distributed).
narrative_ontology:cs_authority_grounding('c536c2d1-2a45-434b-b6bb-6e40a4095491', lineage).
narrative_ontology:cs_interpretation_layer_present('c536c2d1-2a45-434b-b6bb-6e40a4095491').
narrative_ontology:cs_reading_relation('c536c2d1-2a45-434b-b6bb-6e40a4095491', zionist_legitimacy_basis__national_liberation_reading, influences).
narrative_ontology:cs_reading_relation('c536c2d1-2a45-434b-b6bb-6e40a4095491', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('c536c2d1-2a45-434b-b6bb-6e40a4095491', foundational, territorial_retention_theologically_mandated).
narrative_ontology:cs_axiom_status(territorial_retention_theologically_mandated, holdable).
narrative_ontology:cs_axiom_grounding('c536c2d1-2a45-434b-b6bb-6e40a4095491', territorial_retention_theologically_mandated, theological).
narrative_ontology:cs_axiom('c536c2d1-2a45-434b-b6bb-6e40a4095491', foundational, religious_obligation_supersedes_secular_negotiation).
narrative_ontology:cs_axiom_status(religious_obligation_supersedes_secular_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('c536c2d1-2a45-434b-b6bb-6e40a4095491', religious_obligation_supersedes_secular_negotiation, theological).
narrative_ontology:cs_reference_frame('c536c2d1-2a45-434b-b6bb-6e40a4095491', post_1967_redemptive_unfolding).
narrative_ontology:cs_drift_state('c536c2d1-2a45-434b-b6bb-6e40a4095491', post_gaza_disengagement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c536c2d1-2a45-434b-b6bb-6e40a4095491', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, gush_emunim_successor_networks).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, settlement_enterprise_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_land_for_peace_constituency).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, displaced_bedouin_communities).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, covenantal_land_promise_doctrine).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_process_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the 1967 territorial conquest as a divinely ordained stage in messianic redemption and organizes settlement construction, outpost establishment, and political lobbying on that basis. Treats withdrawal from any part of the biblical land as a theological impossibility rather than a policy option, and directs continuous institutional effort (yeshivot hesder, settlement councils, land funds) toward making retreat physically and politically unthinkable.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement, agenda_setter,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement, beneficiary).

% Inherit the founding movement's theological infrastructure and use it to secure state resources, security backing, and legal cover for expansion. Their institutional survival and internal status depend on the covenantal-fulfillment framing remaining authoritative; abandoning it would dissolve their reason for existing.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, gush_emunim_successor_networks, beneficiary,
    organized, generational, identity_locked, regional).

% State ministries, planning authorities, and financing bodies that channel subsidies, infrastructure, and legal recognition toward settlements framed as fulfilling national-religious destiny. They benefit from the theological framing's political durability regardless of their own belief, since it supplies domestic legitimacy that pure security or demographic arguments cannot.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, settlement_enterprise_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Live under a permit, checkpoint, and land-appropriation regime substantially justified by the claim that the territory's disposition answers to a religious mandate that precedes and outranks any negotiated political settlement. Land expropriation, movement restriction, and demolition orders are enforced against them to make settlement expansion irreversible; they have no path to alter the theological premise driving policy over their land.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinian_residents, payer,
    powerless, generational, trapped, local).

% Israeli citizens and political factions who favor territorial compromise for security or diplomatic reasons find negotiating room foreclosed whenever the religious-restoration reading is dominant, since compromise is framed by its adherents as covenant-breaking rather than policy trade-off. They can vote and organize but cannot out-argue a claim structured as non-negotiable religious obligation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_land_for_peace_constituency, payer,
    moderate, biographical, constrained, national).

% Communities in Area C and the Jordan Valley face demolition and relocation pressure tied to settlement expansion justified in part by the messianic-process framing of the land's ultimate disposition; they have no standing within the religious framework that determines the land's fate and no practical route to remain in place.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, displaced_bedouin_communities, payer,
    powerless, generational, trapped, local).

% Issue halachic rulings on the religious status of the territories, the permissibility of withdrawal, and the theological meaning of the 1967 and 1973 wars. Their interpretive authority is the mechanism by which the covenantal reading is transmitted, updated, and enforced within the movement; a ruling against territorial retention would be a doctrinal crisis, not a policy adjustment.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_rabbinic_authorities, observer).

% UN bodies, ICJ opinions, and foreign governments assess the settlements as violations of international law governing occupied territory. Their assessments are treated by the religious-restoration reading as categorically inapplicable, since the framework holds that a divine land grant precedes and is not subject to secular international adjudication; they have no seat within the theological framework that determines the outcome on the ground.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_legal_observers, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides religious-Zionist communities with a coherent theological account of the 1967 and 1973 wars that resolves the tension between traditional exilic quietism and active political-territorial nationalism, coordinating settlement, military service, and political action around a shared messianic narrative.
% TRANSFER_FUNCTION: Moves land, water access, security infrastructure, and state subsidy from Palestinian residents of the West Bank and Jordan Valley toward settlement institutions and their residents, justified as restoration of a divinely promised inheritance rather than as a negotiated territorial claim.
% ABSENT_VOICES: Palestinian residents whose land and movement are directly governed by the practical consequences of this reading have no standing within its theological framework; international legal bodies applying occupation law are treated as categorically irrelevant rather than answered.
% DISAPPEARANCE_RATIONALE: If the religious-restoration framing lost its authority within religious-Zionist politics, the primary non-negotiable theological justification for retaining and expanding settlements would collapse, reopening territorial compromise as a live political option and removing the doctrinal cover that currently forecloses withdrawal from internal debate.
% FOUNDING_PROBLEM: After 1967, religious Zionism needed to explain why a secular, largely non-observant state's military conquest of biblical heartland territory constituted religious progress rather than a further exilic complication — the interpretation resolved this by casting the war as an act of divine redemption unfolding through secular instruments.
% FOUNDING_PROBLEM_CORROBORATION: Religious-Zionist rabbinic authorities and settlement institutions attest the messianic process remains live and ongoing. Secular Israeli historians, former Gush Emunim-adjacent dissenters, and international legal scholars outside the movement attest the theological framing has become primarily a legitimating device for territorial retention that outlived any plausible reading of 1967 as an isolated redemptive event; no consensus corroboration exists from outside the benefiting religious-nationalist camp.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects substantial and growing land, water, and mobility transfers from Palestinian residents to settlement institutions, justified within this reading by theological rather than security or demographic argument. Suppression (0.72) is high and rising because the framework's persistence depends on active enforcement — permit regimes, demolition orders, military backing for outposts — required to make an theologically-declared 'non-negotiable' claim hold on contested ground. Theater ratio is moderate (0.3): much of the settlement and legal apparatus performs genuine administrative and security function, but an increasing share is oriented toward foreclosing negotiation options rather than toward any stated security purpose. Resistance is high (0.75) because the claim is fiercely contested by international law, by secular Israeli political factions, and by the residents whose land is affected. Accessibility collapse is moderate (0.4) — the theological framing has NOT achieved comprehensive alternative-foreclosure at the level of Israeli society as a whole, only within the movement's own institutional and identity structure, which is why resistance remains high rather than dissipated.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the arrangement is not a political program subject to cost-benefit revision but the visible unfolding of covenant — divesting from it is theologically incoherent, not merely costly. From the payer seats, particularly Palestinian residents, the same structure is experienced as an enforcement regime whose justification happens to be theological rather than a difference in kind from any other territorial claim backed by force. The engine's per-seat computation is expected to diverge sharply between these positions precisely because the structural data (power, exit, scope) differs this much across seats, not because of any authored intent to produce divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious settler movement and its rabbinic authorities are the constraint's agenda-setters and primary beneficiaries: they collect political legitimacy, land, state subsidy, and doctrinal authority from the arrangement, and their exit is identity_locked because abandoning the covenantal reading would dissolve the theological basis of their communal and professional identity, not merely change a policy preference. Settlement enterprise institutions benefit organizationally with more mobile exit (arbitrage) since their institutional survival does not require personal theological commitment. West Bank Palestinian residents and displaced Bedouin communities are structural targets — trapped, powerless, bearing the transfer directly on their land and movement. The secular land-for-peace constituency is a softer target: they retain formal political exit (voting, organizing) but find the substantive policy space foreclosed by a claim structured as non-negotiable religious obligation, which is why their exit is constrained rather than trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling secular military conquest with religious meaning after 1967) may have been genuinely live in the years immediately following the Six-Day War, when the theological reading served a real function of integrating unexpected events into religious-Zionist thought. The founding_problem_status is authored as contested rather than dead or live because there is no consensus corroboration: adherents maintain the messianic process is ongoing and therefore the problem remains live by definition (a self-sealing claim), while outside observers read the doctrine's function as having shifted from interpretive integration to territorial-retention justification decades ago. Classifying this as tangled_rope rather than snare preserves the genuine coordination function the reading performs for its own community (resolving real theological tension, providing meaning and purpose) while still registering the asymmetric extraction imposed on non-consenting third parties through the same structure — a pure snare classification would erase the internal coordination function; a pure rope classification would erase the documented victim set and enforcement requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_claim_versus_political_instrumentalization,
    'Is the covenantal-fulfillment reading a genuine, internally-coherent theological tradition that happens to have significant political consequences, or is it primarily a legitimating superstructure deployed by settlement-interested actors over a fundamentally political land claim?',
    'Comparative analysis of religious-Zionist theological writing and rhetoric before 1967 (when the territories were not under Israeli control and the doctrine could not yet be self-serving) versus post-1967 doctrinal development timed against settlement expansion decisions; degree of correlation between doctrinal escalation and specific land-acquisition campaigns would be diagnostic.',
    'If predominantly genuine theology with political consequences, the tangled_rope classification''s coordination-function component is well-grounded. If predominantly instrumentalized legitimation, the constraint is closer to snare — the theological coordination story would be functioning as cover for extraction rather than a real, separable coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_claim_versus_political_instrumentalization, conceptual, 'Whether the religious reading is genuine doctrine or primarily political instrumentalization of doctrine.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the religious-restoration reading''s territorial-maximalism claim logically foreclose the national_liberation_reading''s negotiated-compromise premise within a single religious-Zionist framework, or can both be held by different factions of the same movement without contradiction?',
    'Track whether self-identified religious Zionists who support territorial compromise (e.g., some Meimad-aligned figures) are treated within the movement''s own discourse as heretical/doctrinally incoherent or merely as a minority political position within a shared theological frame.',
    'If treated as doctrinally incoherent, foreclosure is the correct reading_relation to national_liberation_reading for factions holding the full messianic-process view; if treated as a live minority position, coexists_with is more accurate — this omega documents that the answer likely varies by sub-faction within religious Zionism itself, which the single-reading, single-ε discipline of this story cannot resolve internally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether religious-restoration and national-liberation premises can coexist within one religious-Zionist framework or are mutually exclusive.').

omega_variable(
    post_disengagement_doctrinal_crisis,
    'Did the 2005 Gaza disengagement, executed by an Israeli government against religious-Zionist settler resistance, constitute empirical disconfirmation of the theologically-mandated-irreversibility claim, and if so, how did the doctrine survive that disconfirmation?',
    'Examine post-2005 rabbinic responsa and movement literature for doctrinal adaptation (e.g., reframing disengagement as a temporary theological setback within a longer redemptive arc versus treating it as falsifying evidence).',
    'If the doctrine adapted without acknowledging disconfirmation, this supports reading the framework as unfalsifiable by design (raising the axiom_overriding drift-direction concern in cs_structure); if it prompted genuine internal theological revision, the framework shows genuine engagement with counter-evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_disengagement_doctrinal_crisis, empirical, 'Whether the 2005 disengagement functioned as disconfirming evidence the doctrine had to metabolize or was simply absorbed without revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(zion_tr_t1977, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(zion_tr_t2005, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(zion_tr_t2015, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(zion_be_t1977, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(zion_be_t2005, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(zion_be_t2015, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(zion_su_t1977, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(zion_su_t2005, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement(zion_su_t2015, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the zionist_legitimacy_basis kernel. national_liberation_reading and settler_colonial_reading are separate constraint stories with independently authored ε, beneficiary/victim structures, and classifications — they are not alternative measurements of this constraint. This reading's ε (0.68) reflects the extraction attributable to the specific theological-maximalist claim as enforced post-1967; the sibling readings' ε values are authored independently within their own files and should not be assumed to average with or bound this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

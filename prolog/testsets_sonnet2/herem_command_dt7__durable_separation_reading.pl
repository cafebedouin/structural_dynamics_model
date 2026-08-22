% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem as Timeless Mandate for Bounded Covenant Identity (Durable Separation Reading)
 *   domain: religious/ethical/commitment_system
 *
 * SUMMARY:
 *   Deuteronomy 7's herem command instructs Israel to utterly destroy or
 *   categorically separate from seven named Canaanite nations, prohibiting
 *   intermarriage and treating association with outsiders as contamination
 *   risk to covenant fidelity. This story instantiates the
 *   durable_separation_reading: the position that the command encodes a
 *   timeless divine mandate for bounded membership and categorical
 *   separation, not a historically bounded settlement-era directive (the
 *   contextual_supersession_reading) and not a typological allegory of
 *   internal spiritual struggle (the allegorical_displacement_reading). Under
 *   this reading, the text's plain-sense perpetual scope is taken as
 *   authoritative, extending the boundary-maintenance logic to intermarriage
 *   prohibition and outsider designation without expiration. This is the
 *   reading most exposed to violence-legitimation and exclusionary
 *   application in later interpretive history.
 *
 * KEY AGENTS:
 *   - covenant_community_leadership: administers and enforces the boundary (institutional/arbitrage) — primary beneficiary of consolidated interpretive authority
 *   - endogamous_lineage_purity_advocates: benefits from policed membership (organized/mobile)
 *   - designated_outsider_nations: categorical target of the command, no standing to contest (powerless/trapped)
 *   - intermarrying_covenant_members: bear costs of the boundary from inside (moderate/constrained)
 *   - descendants_of_mixed_unions: inherit diminished standing (powerless/trapped)
 *   - biblical_scholars_comparative_ane: analytical observer of the reading's textual and historical warrant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.81).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.78).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Mandate for Bounded Covenant Identity (Durable Separation Reading)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious/ethical/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '5858898b-457a-4eaf-9223-6522e4336e52').
narrative_ontology:cs_kernel_codification('5858898b-457a-4eaf-9223-6522e4336e52', fixed_text).
narrative_ontology:cs_authority_grounding('5858898b-457a-4eaf-9223-6522e4336e52', lineage).
narrative_ontology:cs_interpretation_layer_present('5858898b-457a-4eaf-9223-6522e4336e52').
narrative_ontology:cs_reading_relation('5858898b-457a-4eaf-9223-6522e4336e52', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('5858898b-457a-4eaf-9223-6522e4336e52', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('5858898b-457a-4eaf-9223-6522e4336e52', foundational, herem_mandate_perpetually_binding).
narrative_ontology:cs_axiom_status(herem_mandate_perpetually_binding, holdable).
narrative_ontology:cs_axiom_grounding('5858898b-457a-4eaf-9223-6522e4336e52', herem_mandate_perpetually_binding, deontological).
narrative_ontology:cs_axiom('5858898b-457a-4eaf-9223-6522e4336e52', foundational, named_nations_are_literal_ethnic_referents).
narrative_ontology:cs_axiom_status(named_nations_are_literal_ethnic_referents, holdable).
narrative_ontology:cs_axiom_grounding('5858898b-457a-4eaf-9223-6522e4336e52', named_nations_are_literal_ethnic_referents, conventional).
narrative_ontology:cs_reference_frame('5858898b-457a-4eaf-9223-6522e4336e52', perpetual_covenant_boundary_mandate).
narrative_ontology:cs_drift_state('5858898b-457a-4eaf-9223-6522e4336e52', contemporary_pluralist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5858898b-457a-4eaf-9223-6522e4336e52', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_leadership).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, endogamous_lineage_purity_advocates).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsider_nations).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarrying_covenant_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, descendants_of_mixed_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the boundary rules, adjudicates who counts as inside or outside the covenant, and enforces separation through communal sanction, exclusion from cultic life, and dissolution of mixed marriages. Frames the mandate as timeless and non-negotiable, which consolidates its own authority to police membership indefinitely.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_leadership, beneficiary).

% Gain social status, inheritance security, and cultic standing from the enforced boundary; a stable in-group with policed membership protects their position, land claims, and marriageability against dilution by outsiders.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, endogamous_lineage_purity_advocates, beneficiary,
    organized, generational, mobile, national).

% Named categorically as contamination threats subject to total destruction or exclusion under the mandate; under this reading, their designation as herem targets is treated as a durable command with no expiration, leaving them with no legitimate standing to negotiate exemption or coexistence.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsider_nations, payer,
    powerless, immediate, trapped, regional).

% Members of the covenant community who marry outsiders face dissolution of their marriages, expulsion, or loss of standing under this reading's insistence that categorical separation still binds. Their autonomy over intimate life is subordinated to the boundary-maintenance function.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarrying_covenant_members, payer,
    moderate, biographical, constrained, local).

% Children of mixed unions inherit contested or diminished standing within the community under a durable-separation logic that treats bloodline purity as an ongoing requirement rather than a historically bounded concern.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, descendants_of_mixed_unions, payer,
    powerless, generational, trapped, local).

% Modern religious communities inheriting this text must live with its plain-sense durable-separation reading being cited by some adherents to justify present-day exclusionary or ethno-religious boundary practices; their objections to that application are rarely part of the interpretive authority's official reckoning.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, contemporary_faith_communities, excluded,
    organized, generational, constrained, global).

% Analyze herem within its ancient Near Eastern literary and covenantal context, comparing it to conquest rhetoric conventions of surrounding cultures and assessing whether the text supports a perpetually binding directive or a historically bounded one.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, biblical_scholars_comparative_ane, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, covenant_community_leadership).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bounded covenant identity against absorption or dilution by surrounding populations, preserving cultic exclusivity, land tenure claims, and a distinct communal self-understanding across generations.
% TRANSFER_FUNCTION: Moves social standing, marriageability, inheritance security, and cultic legitimacy toward those inside the policed boundary, while moving physical safety, legal standing, familial stability, and intergenerational status away from those designated outside it or those who cross the boundary through intermarriage.
% ABSENT_VOICES: The designated outsider nations have no voice in the text or its later application — they are named as objects of the command, not parties to it. Descendants of mixed unions and intermarrying members who might argue the boundary should flex are structurally unable to contest their status from within a framework that treats the boundary as timeless.
% DISAPPEARANCE_RATIONALE: Proponents of this reading argue that if the durable-separation mandate were abandoned, covenant identity itself would dissolve into the surrounding culture — the world would rearrange around loss of distinctiveness. Critics counter that covenant identity has in fact persisted and transformed across millennia through communities that reject the perpetually-binding reading, suggesting the world would be largely unchanged by abandoning this specific reading, since alternative identity-preservation mechanisms already coexist with it.
% FOUNDING_PROBLEM: Ancient Israel's settlement-period concern with cultic syncretism and assimilation into surrounding Canaanite religious practice, understood by this reading as continuing indefinitely rather than being tied to that historical moment.
% FOUNDING_PROBLEM_CORROBORATION: Covenant community leadership and purity advocates attest the problem (assimilation-driven identity loss) remains perpetually live. Comparative ANE and historical-critical scholars, writing from outside the beneficiary set, attest the syncretism concern was tied to specific settlement-era political and religious conditions that no longer obtain, and that the perpetually-binding reading is a later theological extension rather than the text's original scope.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because this reading extends the command's force to indefinite scope over intermarriage autonomy and outsider designation — the cost lands on those categorically excluded and on descendants who never had the chance to consent to a settlement-era boundary being applied to them generations later. Suppression (0.78) is high because durability claims foreclose internal reform: if the mandate is timeless, dissent from within the covenant community about its scope is treated as unfaithfulness rather than legitimate reinterpretation. Theater ratio stays comparatively low (0.22) because the enforcement — actual exclusion, marriage dissolution, communal sanction — is substantively real, not merely performative, under this reading's own self-understanding.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting leadership seat, this reading preserves a coordination good — a stable, transmissible identity across threats of assimilation. From the payer seats (outsider nations, intermarrying members, mixed-union descendants), the same structure computes as extraction: categorical exclusion with no appeal, applied without their consent to conditions (ancient Canaanite religious rivalry) that no longer describe their situation. The engine should register this divergence structurally, not resolve it by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Covenant leadership and purity advocates are declared beneficiaries: their standing, authority, and lineage security are enhanced by durable enforcement. Outsider nations, intermarrying members, and mixed-descent individuals are declared victims: the mandate's force falls on them without recourse, and their exit options range from constrained (community members) to trapped (outsiders and descendants who cannot exit a categorical designation assigned at birth or by tribal identity). No override is needed — the beneficiary/victim declarations already track the structural asymmetry the reading itself endorses.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification captures that this reading contains a genuine, non-cynical coordination function (identity preservation against assimilation) that is inseparable, under this reading, from an asymmetric extraction mechanism (permanent categorical exclusion and violence legitimation against named outsiders). Classifying it as pure snare would erase the coordination good the reading itself claims to protect; classifying it as pure rope would erase the victims the reading's own scope creates. The founding_problem interview captures the mandatrophy question directly: outside corroboration (comparative ANE scholarship) suggests the founding problem (settlement-era syncretism) is largely dead, while this reading's own community treats it as still live — that status/verdict mismatch is exactly the zombie-mandate signal the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    durable_vs_bounded_directive,
    'Does Deuteronomy 7''s herem command encode a timeless mandate for categorical separation, or was it a historically bounded directive tied to the specific conditions of Israel''s settlement period (the contextual_supersession_reading)?',
    'Comparative analysis of the text''s internal markers of temporal scope (e.g., references tied to conquest-era geography and named nations that ceased to exist), cross-referencing with how later biblical prophetic and wisdom literature treats outsider relations, and reception history within the tradition''s own interpretive lineage.',
    'If the bounded-directive reading is correct, this constraint''s claimed timelessness is a later theological overlay rather than the text''s original scope, which would substantially reduce the legitimacy basis for extending its extraction to contemporary outsider groups or intermarriage prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durable_vs_bounded_directive, conceptual, 'Whether herem''s scope is perpetual or settlement-era bounded — the core contest between this reading and contextual_supersession_reading.').

omega_variable(
    literal_vs_typological_referent,
    'Are the ''nations'' named in Deuteronomy 7 literal ethnic-political entities subject to a durable separation mandate, or typological placeholders for spiritual/moral threats (the allegorical_displacement_reading)?',
    'Genre analysis of Deuteronomic conquest rhetoric against ANE treaty and conquest-narrative conventions; examination of how the text''s own theological vocabulary (contamination, holiness, election) functions rhetorically versus literally elsewhere in the corpus.',
    'If the typological reading is correct, this reading''s expansive victim set (real ethnic outsiders and their descendants) is a category error — the text''s actual referents would be internal moral struggles, not external populations, collapsing this reading''s extraction claim to near zero.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literal_vs_typological_referent, conceptual, 'Whether the command''s targets are literal populations or spiritual/typological figures — the core contest between this reading and allegorical_displacement_reading.').

omega_variable(
    violence_legitimation_causal_link,
    'Does adopting this reading in a contemporary community causally increase exclusionary or violent application against real outsider groups, or does the reading remain confined to historical-theological discourse without practical effect?',
    'Historical and sociological study of communities that have explicitly held the durable-separation reading, tracking documented instances of exclusionary policy, endogamy enforcement, or violence justified by appeal to this text.',
    'A strong causal link would substantially raise the suppression and extractiveness scores further and would support classifying real-world instantiations closer to snare; a weak link would suggest the reading functions mostly as unexercised doctrine, closer to a piton in practice even while remaining tangled_rope in its own textual logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_legitimation_causal_link, empirical, 'Whether this reading''s adoption has measurable downstream effects on real exclusionary practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t5, herem_command_dt7__durable_separation_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__durable_separation_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(here_tr_t15, herem_command_dt7__durable_separation_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__durable_separation_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__durable_separation_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(here_be_t5, herem_command_dt7__durable_separation_reading, base_extractiveness, 5, 0.73).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__durable_separation_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(here_be_t15, herem_command_dt7__durable_separation_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__durable_separation_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__durable_separation_reading, base_extractiveness, 30, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(here_su_t5, herem_command_dt7__durable_separation_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__durable_separation_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(here_su_t15, herem_command_dt7__durable_separation_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__durable_separation_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__durable_separation_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language label 'the herem command' per the ε-invariance principle: durable_separation_reading (this story, ε=0.81, tangled_rope — timeless mandate, literal referents, expansive victim set), contextual_supersession_reading (settlement-era-bounded, morally superseded, expected much lower ε against contemporary application), and allegorical_displacement_reading (typological reading, expected near-zero ε since no literal victims exist under that framing). Each carries its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged because measuring 'the herem command' by literal-perpetual-scope versus typological-scope versus settlement-bounded-scope yields three structurally distinct ε values, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

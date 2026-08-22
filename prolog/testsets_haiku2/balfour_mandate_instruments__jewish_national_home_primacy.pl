% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate Instruments: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Jewish National Home Primacy'
 *   reading of the contested Balfour Mandate kernel. Under this reading, the
 *   Mandate instruments (League of Nations Mandate for Palestine, 1920;
 *   Articles 2–4) are interpreted to direct demographic and territorial
 *   transformation establishing Jewish political sovereignty. The Jewish
 *   Agency gains quasi-governmental authority (Article 4); land transfer
 *   mechanisms systematically facilitate Jewish ownership accumulation;
 *   immigration quotas privilege Jewish migrants; Arab political
 *   representation is structurally downgraded. The reading asserts that
 *   Mandate text contemplates a proto-state apparatus serving Jewish
 *   institutional supremacy, not merely cultural autonomy or minority
 *   protection. Extraction is high (0.82 at interval end) because the
 *   arrangement transfers territorial control and political authority without
 *   reciprocal Arab institutional development; suppression is substantial
 *   (0.78) because persistence depends on active enforcement of land transfer
 *   regulations, immigration restrictions, and political exclusion. Theater
 *   ratio rises gradually (0.32 to 0.42) as administrative justifications for
 *   enforcement intensify. The claim and metrics are independent: the
 *   constraint is CLAIMED as tangled_rope (coordination + asymmetric
 *   extraction per canonical definition) while the authored metrics describe
 *   highly extractive, actively enforced operation — the engine measures that
 *   independence; do not reconcile.
 *
 * KEY AGENTS:
 *   - jewish_agency: quasi-governmental authority, land and immigration policy-setter (institutional power, generational horizon)
 *   - british_mandatory_authority: formal League of Nations administrator, interpretation authority for Mandate text (institutional power, biographical horizon)
 *   - palestinian_arab_landholders: target of systematic land transfer pressure (powerful power, constrained exit, generational horizon)
 *   - palestinian_arab_political_leadership: structurally downgraded, identity-locked in political frame (moderate power, identity-locked exit)
 *   - international_jewish_organizations: mobilize capital and diplomatic networks to support Jewish Agency (powerful power, global scope)
 *   - arab_state_governments_external: entirely excluded from Mandate governance despite territorial stakes (powerful power, trapped exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.82).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.78).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.82).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '443b99ee-dc3f-42c8-8ab9-3746998f597a').
narrative_ontology:cs_kernel_codification('443b99ee-dc3f-42c8-8ab9-3746998f597a', formalized).
narrative_ontology:cs_authority_grounding('443b99ee-dc3f-42c8-8ab9-3746998f597a', extraction).
narrative_ontology:cs_interpretation_layer_present('443b99ee-dc3f-42c8-8ab9-3746998f597a').
narrative_ontology:cs_reading_relation('443b99ee-dc3f-42c8-8ab9-3746998f597a', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('443b99ee-dc3f-42c8-8ab9-3746998f597a', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('443b99ee-dc3f-42c8-8ab9-3746998f597a', foundational, jewish_historical_territorial_claim_primacy).
narrative_ontology:cs_axiom_status(jewish_historical_territorial_claim_primacy, holdable).
narrative_ontology:cs_axiom_grounding('443b99ee-dc3f-42c8-8ab9-3746998f597a', jewish_historical_territorial_claim_primacy, deontological).
narrative_ontology:cs_axiom('443b99ee-dc3f-42c8-8ab9-3746998f597a', foundational, zionist_institutional_supremacy_as_mandate_requirement).
narrative_ontology:cs_axiom_status(zionist_institutional_supremacy_as_mandate_requirement, holdable).
narrative_ontology:cs_axiom_grounding('443b99ee-dc3f-42c8-8ab9-3746998f597a', zionist_institutional_supremacy_as_mandate_requirement, instrumental).
narrative_ontology:cs_axiom('443b99ee-dc3f-42c8-8ab9-3746998f597a', secondary, demographic_transformation_institutional_necessity).
narrative_ontology:cs_axiom_status(demographic_transformation_institutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('443b99ee-dc3f-42c8-8ab9-3746998f597a', demographic_transformation_institutional_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('443b99ee-dc3f-42c8-8ab9-3746998f597a', jewish_political_sovereignty_establishment_framework).
narrative_ontology:cs_drift_state('443b99ee-dc3f-42c8-8ab9-3746998f597a', post_1948_state_establishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('443b99ee-dc3f-42c8-8ab9-3746998f597a', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants_settlement_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, international_jewish_organizations).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, global_jewish_diaspora_communities).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, jewish_historical_claim_to_territorial_sovereignty).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates as a quasi-governmental authority under Mandate Article 4, setting land purchase policy, immigration quotas, and institutional development priorities. Controls settlement expansion, educational institutions, and defense force recruitment. Represents organized Jewish population and coordinates with Zionist political movements globally. Acts as the administrative architect of demographic transformation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive facilitated immigration, subsidized land purchase through Jewish National Fund, and institutional support for agricultural settlement and urban development. Flow of new settlers accelerates demographic shift; land purchase networks are structured to maximize Jewish ownership accumulation. Obtain security guarantees and administrative privilege unavailable to Arab population.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants_settlement_organizations, beneficiary,
    organized, biographical, mobile, national).

% Holds formal administrative authority under League of Nations mandate. Interprets Mandate text to prioritize Jewish national home construction; enforces land transfer regulations, immigration quotas, and security arrangements that favor Jewish institutional development. Benefits from Zionist institutional cooperation in administrative efficiency and from strategic positioning in regional geopolitics. Constrained by League oversight (nominal) and mounting Arab resistance.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority, beneficiary).

% Face systematic land purchase pressure from Jewish organizations backed by international capital and institutional coordination. Land sales are facilitated by British regulatory structures (e.g., Absentee Property Law implementation) that favor Jewish buyers. Landholding communities are fragmented; economic incentives and social pressure segment landholders into sellers and resisters. Exit option is ceding territorial control; remaining is bearing the cost of demographic displacement.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerful, generational, constrained, national).

% Structurally downgraded in Mandate governance hierarchy; excluded from policy-setting on land, immigration, and institutional development. Hold nominal representation in advisory bodies with no enforcement power. Identity-locked by national and religious commitment to territorial control and population sovereignty; exit from the political frame is organizational dissolution. Bear the cost of institutional subordination and demographic transformation without authority to redirect it.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    moderate, generational, identity_locked, national).

% Receives Mandate reports from the British authority; provides nominal oversight and receives petitions from Arab organizations. Lacks enforcement mechanisms to compel interpretation disputes or remedy implementation complaints. Treats Mandate text as delegating interpretive authority to the mandatory power; supervision is post-hoc documentary, not prospective policy review.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_supervisory_body, observer,
    institutional, biographical, analytical, universal).

% Mobilize capital, diplomatic recognition, and institutional networks to support Jewish Agency operations; coordinate with Zionist political movements in multiple countries. Provide funding for land purchases, settlement development, and institutional establishment. Exercise influence over British policy through diplomatic channels and public advocacy. Benefit from institutional framework that facilitates their policy agenda.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, international_jewish_organizations, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, international_jewish_organizations, agenda_setter).

% Neighboring Arab states have no formal role in Mandate governance or policy-setting despite territorial and demographic implications. Excluded from negotiations on land transfer, immigration quotas, or institutional arrangements. Would oppose Jewish institutional supremacy and demographic transformation if admitted to policy deliberation; their exclusion is maintained by the Mandate structure itself.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, arab_state_governments_external, excluded,
    powerful, generational, trapped, national).

% Benefit from the establishment of a Jewish political and territorial center; gain migration option, institutional identity anchor, and collective political representation potential. Investment communities provide capital for settlement and institutional development. Benefit from the expansion of Jewish institutional capacity and sovereign presence.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, global_jewish_diaspora_communities, beneficiary,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mandate instruments coordinate the establishment of Jewish political institutions, land administration systems, and demographic settlement infrastructure within a defined territorial framework; they solve the collective-action problem of building state capacity and territorial control through centralized institutional direction and capital mobilization.
% TRANSFER_FUNCTION: Transfers territorial control and political authority from Arab population and Ottoman/Arab administrative systems to Jewish institutional structures; moves land ownership from Arab landholders to Jewish organizations and settlers; redirects immigration flows to privilege Jewish migrants over other populations; channels international capital and institutional support to Jewish settlement and state-building rather than to Arab institutional development.
% ABSENT_VOICES: Palestinian Arab landholders and political leadership are structurally minimized in policy-setting (represented only in advisory capacity); Arab state governments are entirely excluded from Mandate governance despite territorial adjacency and demographic stakes; global Islamic institutional frameworks are absent from deliberations over religious property and minority protections; anti-Zionist Jewish organizations are excluded from the Jewish Agency's agenda-setting role despite representing alternative visions of Jewish political organization.
% DISAPPEARANCE_RATIONALE: If the Mandate instruments and their Jewish-primacy interpretation vanished, territorial control would devolve to Arab-majority institutions, land ownership patterns would be governed by Arab political frameworks, immigration policy would not systematically privilege Jewish migrants, and the subsequent state institutional structure would reflect Arab demographic and political precedence. The entire apparatus of Jewish state-building under this reading depends on the Mandate framework and the British authority's enforcement of the Jewish-primacy interpretation.
% FOUNDING_PROBLEM: Post-WWI empire reorganization and the Balfour Declaration (1917) created a legal and political framework to enable Jewish political organization and territorial establishment in Palestine in fulfillment of the Zionist movement's sovereignty objectives, while managing the disposition of Ottoman territories among Allied powers.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutional leadership and the British Mandate authority attest the founding problem as live: establishing Jewish political institutions and territorial control requires continued demographic transformation and land accumulation. Palestinian Arab political leadership disputes this framing, attesting that the founding problem was a colonial-power construction serving imperial strategy, not a genuine coordination challenge. International historians and legal scholars from outside both communities document that the Mandate structure systematically privileged Jewish institutional development and downgraded Arab self-determination claims from its inception (e.g., David Fromkin's 'A Peace to End All Peace,' Susan Abulhawa's scholarship on Mandate legal instruments).
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises throughout the interval (0.68→0.82) as land purchase mechanisms accelerate, immigration quotas tighten in Jewish favor, and institutional apparatus solidifies. Suppression requirement increases (0.64→0.78) as Arab resistance to demographic transformation intensifies and British enforcement of land transfer rules must intensify to maintain the apparatus. Theater ratio rises gradually (0.32→0.42) as the administrative narrative of 'development' and 'security' expands to justify enforcement; early justifications emphasize institutional establishment and legal framework, later justifications emphasize security and administrative necessity. The three series share one time grid (0, 5, 10, 15, 20, 25, 30) so drift is temporally aligned. Seat divergence: from the Jewish Agency and British Mandate authority seat, the arrangement is genuine coordination (establishing state capacity, managing territory, facilitating migration within a legal framework). From Palestinian Arab seats, the same structure operates as enforced extraction of territorial control and political authority. The engine computes this divergence from the structural data (beneficiary/victim declarations, exit options, power atoms).
 *
 * PERSPECTIVAL GAP:
 *   The Jewish Agency and British Mandate authority seats should compute as experiencing tangled_rope: genuine coordination (state apparatus, legal institutions, capital mobilization, demographic settlement) delivering to beneficiaries, with asymmetric extraction as the cost borne by Arab seats. Palestinian Arab seats should compute as experiencing snare: the coordination story is cover; persistence depends on coercion (land transfer enforcement, political exclusion, immigration controls) and suppression of exits and alternatives (Arab institutional development, territorial autonomy). The engine derives per-seat type from power, exit options, and beneficiary/victim data; this structural divergence is the measurement the corpus exists to take. Commentary documents the divergence; the engine computes it from the authored data.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish Agency: beneficiary, agenda-setter, institutional power → d ≈ 0.15 (full beneficiary pole, derives maximum coordination and institutional control benefit). Jewish migrants: beneficiary, organized power, mobile exit → d ≈ 0.20 (beneficiary, but individual agents have mobility option). British Mandate authority: dual-positioned (agenda-setter, secondary beneficiary) → d ≈ 0.25 (benefits from administrative cooperation and strategic positioning, but exit is constrained by League oversight and Arab resistance; overrides may be warranted if the story intends to highlight capture dynamics). Palestinian Arab landholders: victims, powerful power but constrained exit → d ≈ 0.82 (high target end: bear territorial loss, constrained alternatives). Palestinian Arab political leadership: victims, moderate power, identity-locked exit → d ≈ 0.88 (highest target end: identity-fusion to territorial and political frame makes exit psychologically/politically impossible; bear institutional subordination as structural feature). International Jewish organizations: beneficiary, powerful power, arbitrage exit → d ≈ 0.18 (beneficiary, mobile capital and institutional networks). Arab state governments: excluded, trapped exit → d ≈ 0.85 (high target end, but excluded from direct extraction; trapped by geopolitical configuration and the Mandate structure itself).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy arises when a constraint's founding justification no longer maps to its structure. In this reading, the founding problem (post-WWI territorial reorganization and Zionist state-building) is described as live by both the Jewish Agency and the Palestinian Arab political leadership — but they dispute what solving it means. The Jewish Agency reads it as requiring continued demographic transformation and territorial accumulation. Palestinian Arab leadership reads it as a colonial construction that was never legitimate and should be replaced with Arab self-determination. The mismatch between founding_problem_status=live and the high extractiveness/suppression metrics suggests that persistence is increasingly maintained by institutional inertia and enforcement capacity rather than by genuine coordination value. The rising theater_ratio (0.32→0.42) supports this: administrative justifications for enforcement expand as the founding problem diverges from observable conditions. Per the schema, mandatrophy is a property the story describes, not a value to be authored — the story documents the mismatch; the engine flags it via the T17 hypothesis if the mismatch meets thresholds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_ambiguity_reading_frame,
    'Does the Mandate text itself support a single ''correct'' reading of Jewish national home vs. Arab civil/political rights, or does the text structurally permit alternative readings depending on prior normative commitments?',
    'Formal textual analysis comparing: (1) the exact wording of Articles 2–4, 22–26 in original League documents; (2) contemporaneous preparatory discussions and Balfour Declaration text; (3) contemporary legal scholarship identifying linguistic ambiguities, gaps, and interpretive entry points. The ambiguity may be irreducible (the text may genuinely not settle the reading).',
    'If the text is genuinely ambiguous, then the choice to privilege the Jewish-primacy reading is a policy decision, not a textual discovery — the reading''s ''naturalness'' dissolves and the constraint becomes more clearly tangled_rope (coordination vehicle for a contested policy). If the text clearly supports one reading, the other reading is untenable within the interpretive framework and forecloses it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_text_ambiguity_reading_frame, conceptual, 'Whether Mandate text permits or forecloses alternative readings via its own linguistic/structural properties.').

omega_variable(
    jewish_agency_quasi_governmental_status_ambiguity,
    'Does Article 4''s grant of quasi-governmental status to the Jewish Agency constitute authorization for the Agency to set demographic and territorial policy independent of Arab political representation, or does it constitute authorization only for internal Jewish institutional administration subject to British democratic/representative accountability?',
    'Close reading of Article 4 text and legal commentary on the scope of ''quasi-governmental'' authority; comparison with comparable League Mandate provisions granting administrative delegation; examination of British administrative practice (did the Agency set policy unilaterally, or did British authority retain policy-setting authority and delegate only implementation?); testimony from administrators and Jewish Agency officials on the actual decision-making process.',
    'If Article 4 authorizes unilateral Agency policy-setting, the reading''s institutional supremacy claim is text-supported. If Article 4 merely delegates implementation, then the reading''s interpretation overstates the Agency''s autonomous authority and the constraint reflects British policy choices more than the Mandate text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_agency_quasi_governmental_status_ambiguity, empirical, 'The scope of Jewish Agency authority under Article 4 of the Mandate.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.78 at interval end) primarily structural (legal barriers to land sale, immigration quotas enforced by external authority) or internalized (Palestinian Arab political leadership''s own strategic choices and adaptive expectations regarding what outcomes are achievable)?',
    'Comparative analysis of Palestinian Arab political positions and resistance tactics across the interval (0–30); examination of explicit statements by Arab leaders regarding feasible vs. foreclosed options; counterfactual analysis (if legal barriers were removed, would Palestinian Arab institutional development and land retention accelerate?); post-exit suppression trajectory (if Arab institutional authority were established, would suppression persist in behavioral patterns).',
    'If suppression is primarily structural, removing the Mandate constraint would rapidly enable Arab institutional development and territorial control. If suppression is substantially internalized, Palestinian Arab institutions would continue to encounter obstacles to territorial consolidation even without the Mandate apparatus — the constraint carries psychological/strategic embedding. High internalization would suggest that the constraint''s effective extraction exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Mechanism of suppression: structural barriers vs. internalized political expectations.').

omega_variable(
    committer_frame_sibling_readings_relationship,
    'What is the actual historical and logical relationship between this reading (Jewish-national-home primacy) and the alternative readings (dual-obligation, mandatory-interpretive-discretion)? Did they emerge as coherent contemporaneous positions, or does the retrospective framing of ''coexists_with'' and ''forecloses'' impose a logical structure that post-dates the actual historical contestation?',
    'Historical reconstruction: identify which actors articulated which readings at which historical moments; examine whether the readings were framed as competing interpretations or whether the contest was framed in different terms (British colonial interest, Arab nationalism, Zionist aspirations). The retrospective imposition of clean logical relationships may obscure the actual messiness of the historical process.',
    'If the readings are post-hoc reconstructions rather than contemporaneous articulations, the committer frame may oversimplify the kernel''s actual contestation. The engine''s computation of foreclosure/coexistence relationships would be driven by a narrative structure rather than by the historical actors'' own understanding of the stakes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_sibling_readings_relationship, conceptual, 'Whether the articulated sibling readings represent historical actors'' actual positions or retrospective logical reconstruction.').

omega_variable(
    founding_problem_legitimacy_and_consent,
    'Was the founding problem (post-WWI territorial reorganization and Zionist state-building) framed and accepted as a legitimate problem to solve by the Palestinian Arab population whose territorial and political future was being transformed, or was it imposed as a problem-framing by external powers and Zionist institutions?',
    'Historical documentation: Palestinian Arab statements and positions at the time of the Mandate establishment; contemporary Arab press, political party platforms, and organizational statements; transcripts of negotiations or consultations regarding the Mandate''s terms. The answer will likely show that the founding problem was NOT acknowledged as legitimate by Palestinian Arab constituencies.',
    'If the founding problem was externally imposed rather than consensually defined, then the founding_problem_corroboration field must acknowledge that corroboration exists only from the reading''s beneficiary parties (Jewish Agency, British authority) and their aligned constituencies. The constraint would then lack the cross-party validation that typically supports coordination claims, reinforcing the tangled_rope classification and suggesting snare dynamics from the Palestinian Arab perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_legitimacy_and_consent, empirical, 'Whether the founding problem was consensually defined or externally imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_mandate_jewish_home_tr_t0, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_tr_t0, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_tr_t5, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_tr_t5, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_tr_t10, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_tr_t10, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_tr_t15, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_tr_t15, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_tr_t20, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_tr_t20, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_tr_t25, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_tr_t25, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_tr_t30, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(balfour_mandate_jewish_home_be_t0, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_be_t0, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_be_t5, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_be_t5, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_be_t10, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_be_t10, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_be_t15, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_be_t15, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_be_t20, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_be_t20, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_be_t25, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_be_t25, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_be_t30, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(balfour_mandate_jewish_home_su_t0, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_su_t0, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_su_t5, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_su_t5, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_su_t10, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_su_t10, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_su_t15, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_su_t15, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_su_t20, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_su_t20, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_su_t25, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_su_t25, observed).
narrative_ontology:measurement(balfour_mandate_jewish_home_su_t30, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(balfour_mandate_jewish_home_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, identity_coordination).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__jewish_national_home_primacy, 0.18).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested 'balfour_mandate_instruments' kernel. The Jewish-national-home-primacy reading interprets the Mandate to direct demographic and territorial transformation establishing Jewish political sovereignty. The dual-obligation-indigenous-rights reading interprets the Mandate to impose equal or superior obligation to protect existing Arab civil/political rights and land tenure. The mandatory-interpretive-discretion reading treats the British authority's interpretive discretion itself as the operational constraint. All three are linked via network.affects_constraints; the engine computes which reading's operative constraint structure is realized through the power and exit-option data, not through text-internal properties alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

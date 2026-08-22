% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Balfour Mandate: Jewish National Home Primacy (Zionist Sovereignty Reading)
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The Balfour Mandate instruments (1920–1948) embodied a contested
 *   commitment: whether the League of Nations Mandate system in Palestine
 *   served primarily to establish a Jewish 'national home' on the path to
 *   sovereignty, or to protect the civil and political rights of the existing
 *   Arab population while facilitating Jewish settlement as a secondary
 *   consideration. This constraint story instantiates the FIRST reading: the
 *   'national home' is interpreted as proto-statehood requiring land access,
 *   immigration facilitation, and Jewish institutional supremacy. Under this
 *   reading, the Mandate instruments operate as a tangled-rope structure
 *   coordinating the construction of Jewish state institutions while
 *   simultaneously extracting territorial control and political voice from
 *   Palestinian Arabs. The British mandatory authority enables this
 *   extraction by interpreting its discretionary power to prioritize Zionist
 *   settlement facilitation. Beneficiaries are Zionist institutions and
 *   Jewish migrants; victims are Palestinian Arab landholders and political
 *   leadership.
 *
 * KEY AGENTS:
 *   - Jewish Agency / Zionist Institutions: agenda-setter, organized power, coordinates settlement and institutional development
 *   - Palestinian Arab Landholders: victims, powerless locally, trapped in exit options, face systematic land acquisition pressure
 *   - Palestinian Arab Political Leadership: victims, moderate power regionally, structurally excluded from decision-making, constrained exit
 *   - British Mandatory Authority: institutional power, dual role (agenda-setter + payer), interprets Mandate to prioritize Zionist preferences
 *   - Jewish Migrants/Settlers: beneficiaries, moderate power, gain facilitated immigration and settlement support
 *   - League of Nations Oversight: analytical observer position, lacks enforcement authority
 *   - Jewish Diaspora International Community: beneficiary, powerful, mobilizes economic and diplomatic resources
 *   - International Criticism Voices: excluded (Arab League, anti-colonial movements), constrained from affecting decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.81).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.76).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.81).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate: Jewish National Home Primacy (Zionist Sovereignty Reading)").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '12bb2fe3-044d-4dad-b050-bcc62ba0be76').
narrative_ontology:cs_kernel_codification('12bb2fe3-044d-4dad-b050-bcc62ba0be76', fixed_text).
narrative_ontology:cs_authority_grounding('12bb2fe3-044d-4dad-b050-bcc62ba0be76', extraction).
narrative_ontology:cs_interpretation_layer_present('12bb2fe3-044d-4dad-b050-bcc62ba0be76').
narrative_ontology:cs_reading_relation('12bb2fe3-044d-4dad-b050-bcc62ba0be76', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('12bb2fe3-044d-4dad-b050-bcc62ba0be76', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('12bb2fe3-044d-4dad-b050-bcc62ba0be76', foundational, national_home_implies_territorial_supremacy).
narrative_ontology:cs_axiom_status(national_home_implies_territorial_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('12bb2fe3-044d-4dad-b050-bcc62ba0be76', national_home_implies_territorial_supremacy, deontological).
narrative_ontology:cs_axiom('12bb2fe3-044d-4dad-b050-bcc62ba0be76', foundational, jewish_settlement_facilitation_primary_mandate_function).
narrative_ontology:cs_axiom_status(jewish_settlement_facilitation_primary_mandate_function, holdable).
narrative_ontology:cs_axiom_grounding('12bb2fe3-044d-4dad-b050-bcc62ba0be76', jewish_settlement_facilitation_primary_mandate_function, empirically_contingent).
narrative_ontology:cs_reference_frame('12bb2fe3-044d-4dad-b050-bcc62ba0be76', balfour_commitment_territorial_jewish_sovereignty).
narrative_ontology:cs_drift_state('12bb2fe3-044d-4dad-b050-bcc62ba0be76', post_1945_decolonization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('12bb2fe3-044d-4dad-b050-bcc62ba0be76', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency_zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants_settlers).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_diaspora_international_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Mandate as authorization to build state institutions parallel to or superseding the mandatory power's colonial administration. Negotiates immigration quotas, land purchase protocols, and control of internal Jewish community affairs. Operates quasi-governmental functions (labor arbitration, internal taxation, education) with tacit British recognition. Strategic exit option: shift resource mobilization and settlement focus to neighboring territories or invoke external pressure from diaspora Jewry to force British compliance with the interpretation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency_zionist_institutions, agenda_setter,
    organized, generational, arbitrage, global).

% Gain systematic facilitation of immigration through Jewish Agency quota negotiation, preferential land purchase arrangements, and institutional support for settlement. Their labor and capital drive territorial acquisition and demographic change. Exit: return to diaspora, relocate to alternative settlement zones, or shift support to rival political factions within Jewry—but the reading's framing makes territorial permanence the goal, which constrains exit.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants_settlers, beneficiary,
    moderate, biographical, arbitrage, global).

% Holds formal sovereignty under League of Nations Mandate but interprets it as permitting—even requiring—facilitation of Jewish national-home construction. Issues immigration certificates, validates land transfers, and enforces restrictions on Arab political organization. Bears administrative costs and international criticism but gains strategic territorial position, Jewish community cooperation, and extraction of mandate revenues. Trapped between contradictory mandate obligations (protection of existing inhabitants vs. establishment of national home) and resolves through prioritizing Zionist preferences.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority, payer).

% Face systematic pressure and facilitation of land sales to Jewish purchasers through British administrative machinery, Jewish Agency credit systems, and social pressure from within Arab communities. Loss of land constitutes loss of livelihood base and ancestral territorial claim. Exit options are severely constrained: refusal to sell triggers economic isolation or administrative targeting; sale severs economic security; migration is restricted by the Mandate's immigration quotas privileging Jews. Resistance is localized and easily suppressed by mandatory authority.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, biographical, trapped, local).

% Structurally excluded from legislative and executive roles in the Mandate administration. Their political voice is subordinated to advisory bodies with no binding authority. They are consulted on matters the mandatory authority chooses, but the Mandate's structure ensures that decisions on immigration, land, and Jewish institutional development are not subject to their veto. Their costs: erosion of political legitimacy, loss of territorial control, demographic shift undermining long-term viability of Arab statehood. Exit: organizational defection, armed resistance, or international appeals—but all are constrained by the mandatory power's monopoly on enforcement and by the Mandate text's own language privileging the Jewish national home.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    moderate, generational, constrained, regional).

% Receive annual reports from the mandatory power but lack enforcement authority over interpretation disputes. They witness the constraint's operation and can issue findings, but the Mandate text's ambiguity and the mandatory power's discretion mean that oversight is largely post-hoc commentary. Their analytical position reveals but does not adjudicate the constraint's bifurcation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_oversight_bodies, observer,
    institutional, generational, analytical, universal).

% Mobilizes economic and diplomatic resources to support Jewish migration and settlement, viewing the Mandate as a historic opportunity for Jewish sovereignty and refuge. Applies political pressure to the mandatory authority and League when settlement policies are perceived as insufficient. Their leverage derives from capital flows, access to international forums, and narrative control over the Mandate's legitimacy. Can shift resources to alternative political vehicles or rival Zionist factions if this reading loses organizational dominance.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_diaspora_international_community, beneficiary,
    powerful, generational, arbitrage, global).

% Arab League states, anti-colonial movements, humanitarian advocates, and some League member states object to the reading's prioritization of Zionist settlement over indigenous Arab rights. Their criticism is excluded from the Mandate's decision-making structure; they witness and resist but do not adjudicate. Their exclusion is maintained by the Mandate text's silence on Arab political voice and by Britain's enforcement machinery.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, international_criticism_voices, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency_zionist_institutions).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal-administrative framework for building parallel Jewish institutional structures (labor organization, education, land purchase coordination, internal governance) within the Mandate territory. Under this reading, the coordination problem solved is: how to enable Jewish national institutions to organize community affairs and territorial acquisition without independent statehood machinery. The Mandate provides the framework; the Jewish Agency operationalizes it.
% TRANSFER_FUNCTION: Transfers territorial control, immigration rights, and institutional autonomy from the pre-Mandate Arab territorial order and from the mandatory power's unilateral discretion to the Jewish Agency and Zionist settlement structures. Specifically: Palestinian Arab landholders transfer property to Jewish purchasers through facilitated mechanisms; Arab political leadership loses veto authority on immigration and land policy; Jewish migrants gain guaranteed access; the mandatory authority gains strategic position and cooperation from organized Jewish institutions in exchange for ceding regulatory discretion on Zionist priorities.
% ABSENT_VOICES: Palestinian Arabs are structurally excluded from the Mandate's legislative and executive bodies. Their objections to the reading—that 'national home' should be subordinated to indigenous rights protection—are raised but not heard in the decision-making seats. Anti-colonial international voices and Arab League states object to the prioritization but lack standing in the Mandate's institutional structure.
% DISAPPEARANCE_RATIONALE: If the Mandate instruments and their reading disappeared overnight, the territorial transformation machinery would stop. Without the legal framework facilitating land sales, immigration quotas, and Jewish institutional supremacy, settlement dynamics would shift dramatically: Palestinian Arab property would remain under Arab control or British administrative management; Jewish immigration would rely on independent negotiation rather than quota facilitation; institutional governance would revert to unilateral British administration without parallel Jewish Agency structures. The world that emerges would be recognizable but substantially altered—territorial control, demographic composition, and institutional authority would track different lines.
% FOUNDING_PROBLEM: Post-WWI imperial necessity of organizing former Ottoman territories under League supervision; British strategic interest in a cooperative organized community within Palestine; Zionist claims to historical connection and need for refuge from European antisemitism; and Balfour Declaration's 1917 commitment to a Jewish 'national home' in Palestine. The founding problem, under this reading, is: how to operationalize the Balfour commitment while maintaining British strategic control and navigating the presence of an existing Arab population.
% FOUNDING_PROBLEM_CORROBORATION: The British mandatory authority and Zionist leadership attest the founding problem as a live coordination challenge they are solving through the Mandate instruments. However, independent historical analysis (including League of Nations records, British Colonial Office minutes, and later-commissioned studies like the Peel Commission) establishes that the founding problem, under this reading, was constructed to justify a predetermined political outcome—establishing Jewish institutional supremacy was a choice, not a necessity dictated by the mandate language itself. Palestinian Arab leadership and anti-colonial analysts provide corroborating testimony that the 'founding problem' framework was used to subordinate indigenous rights to settlement priorities.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81 terminal value) because the constraint's operation systematically transfers territorial control, political voice, and resource access from Palestinian Arabs to Zionist institutions through facilitated mechanisms—land sales, immigration quotas, institutional autonomy—that are not reciprocal. The trend is upward (0.61→0.81 over the interval) because systematic settlement accelerates, making the extraction more comprehensive. Suppression is high (0.76) because Palestinian Arab resistance to land sales and political subordination is actively suppressed by British enforcement machinery (police, administrative restrictions, exclusion from decision-making). Theater is moderate-low (0.28) because while the Mandate uses coordination language ('national home,' 'protection of existing inhabitants'), a growing share of actual administrative action defends the settlement extraction rather than the stated coordination. The measurement series tracks one shared time grid: every metric is authored at every examined point (0, 5, 10, 15, 20, 25, 30), enabling temporal analysis of extraction accumulation and suppression intensification.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is extreme. From the British mandatory authority and Jewish Agency seats, the constraint appears as coordination—building Jewish institutions and enabling settlement is a legitimate mandate interpretation that solves real organizational problems (how to build state capacity, how to facilitate migration). From the Palestinian Arab seats (landholders and political leadership), the same constraint operates as enforced extraction: their land is taken, their political voice is nullified, their exit options are blocked. The engine computes this divergence from the structural data: the Jewish Agency holds organized power and arbitrage exit (can shift resources or threaten international pressure); Palestinian Arabs hold powerless/moderate power and trapped/constrained exit. Different power atoms feed different directionality values, which produce different type classifications per seat. The claim (tangled_rope) reflects the structure: genuine coordination on the Zionist side married to asymmetric extraction on the Arab side, both riding the same institutional machinery.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values form a sharp spectrum. Jewish Agency/Institutions: d ≈ 0.15–0.25 (beneficiary end). They benefit from facilitated settlement, gain quasi-governmental status, and can arbitrage exit (redirect resources or leverage international pressure if British policy shifts). Jewish Migrants: d ≈ 0.20–0.35 (beneficiary end leaning toward symmetric). They gain settlement facilitation but remain dependent on continued immigration quotas and institutional support; moderate exit optionality (can return to diaspora). British Mandatory Authority: d ≈ 0.45–0.55 (symmetric). They coordinate state functions but absorb costs (administrative burden, international criticism, armed resistance) and are trapped in contradictory mandate obligations. Palestinian Arab Landholders: d ≈ 0.85–0.92 (target end). They bear extraction (land loss, economic displacement), hold powerless local power, and face trapped exit (sell and lose livelihood, refuse and face isolation, migrate and face immigration restrictions). Palestinian Arab Political Leadership: d ≈ 0.78–0.88 (near-target end). They bear extraction (political voice nullification, territorial loss of governance), hold moderate regional power but constrained local authority, and face constrained exit (resistance is easily suppressed, international appeals have limited force). These divergent values reflect real structural asymmetry: the same constraint sits beneficially for organizers and extractively for targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is contested: British and Zionist seats attest the problem is live and ongoing, while Palestinian Arab leadership and later historians attest the problem is now dead (Zionist settlement is established, Jewish institutions are operational) but the constraint persists as rent collection. The mandatrophy flag fires on this mismatch: founding_problem_status=contested + disappearance_verdict=world_rearranges + theater_ratio trending upward suggests mandate obsolescence. The constraint is maintaining Jewish institutional supremacy and settlement extraction not because the original coordination problem persists, but because beneficiary seats (Jewish Agency, British authority) extract value from it. This is the classic mandatrophy signature: the coordination justification has outlived its function, and what remains is pure extraction dressed in coordination language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_ambiguity,
    'Is the Mandate text''s phrase ''national home'' sufficiently clear to constrain interpretation, or does it remain inherently ambiguous between ''settlement destination'' (this reading) and ''protected minority rights'' (sibling reading)?',
    'Linguistic and historical analysis of the Balfour Declaration, the Mandate text, contemporary diplomatic correspondence, and League of Nations committee proceedings. Examine whether framers explicitly addressed the tension, and if so, what did they settle.',
    'If the text is genuinely ambiguous, this reading and its sibling both remain defensible; if the text was framed to privilege ''national home'' as proto-statehood, this reading gains textual grounding; if framed to subordinate it to indigenous rights, this reading is texturally vulnerable. Textual clarity determines whether the constraint is a reading dispute or a violation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_text_ambiguity, empirical, 'Whether Mandate text ambiguity enables or forecloses this reading').

omega_variable(
    coordination_extraction_separability,
    'Are the genuine coordination functions (institution-building, settlement facilitation for Jewish migrants) structurally separable from the extraction mechanisms (land acquisition pressure, Arab political subordination), or are they inseparable?',
    'Counterfactual analysis: could Jewish institutions have been built and migration facilitated WITHOUT systematically subordinating Arab political voice and acquiring Arab land? Examine actual policy choices (e.g., restricting immigration timing, prioritizing mixed municipal governments, limiting land-purchase facilitation) versus the choices made.',
    'If separable, the high extraction is a policy choice the constraint enables, not an intrinsic requirement—the constraint becomes classifiable as snare rather than tangled_rope. If inseparable, the tangled_rope classification holds: coordination and extraction are structurally bound. This is the mandatrophy crux: was the extraction serving the coordination, or did the coordination become a cover story for extraction?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction functions are structurally inseparable or policy-separable').

omega_variable(
    suppression_internalization_mechanism,
    'Is Palestinian Arab political quiescence a result of structural suppression (police force, administrative barriers, exclusion from decision machinery), internalized psychological acceptance (Arab leaders believed the Mandate would eventually permit self-determination), or a combination?',
    'Post-constraint analysis: after the Mandate instruments are removed or their interpretation shifts, does Arab political resistance persist at the same intensity, escalate (suggesting it was internalized and now released), or transform (suggesting suppression was partially structural, partially internalized)? Oral histories and archival evidence from Arab nationalist movements during the Mandate period.',
    'If largely internalized, the measured suppression underestimates the actual coercive force—the constraint''s effective suppression persists even after formal mechanisms are removed. If largely structural, removing the Mandate machinery should substantially reduce resistance (if it doesn''t, internalization has occurred). This affects the classification: higher internalized suppression moves the constraint toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Extent of suppression internalization in Palestinian Arab political quiescence').

omega_variable(
    reading_contest_in_british_discretion,
    'Did the British mandatory authority deliberately choose this reading (jewish_national_home_primacy) over the dual_obligation reading, or did it drift into this reading through incremental decisions that accumulated into a pattern?',
    'British Colonial Office minutes, correspondence with League of Nations, explicit policy directives on land, immigration quotas, and Arab political representation. Distinguish between stated policy (what was decided in London) and practice (what actually happened on the ground), and between deliberate choices and administrative drift.',
    'If deliberately chosen, the British authority is an intentional beneficiary-seat enforcer of the reading; if drifted into, the constraint may be partially emergent rather than enacted. This affects the characterization of british_mandatory_authority''s role: deliberate choice → agenda_setter role is stronger; drift → agenda_setter role is weaker, more payer-ish. Affects mandatrophy interpretation: deliberate maintenance suggests capture; drift suggests institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_in_british_discretion, empirical, 'Whether British policy was deliberate choice or administrative drift').

omega_variable(
    kernel_contest_foreclosure,
    'Does this reading''s core premise (Jewish national home as proto-statehood requiring supremacy) logically foreclose the dual_obligation_indigenous_rights reading, or do both readings remain simultaneously defensible from different interpretive frames?',
    'Formal logic analysis: map the core premises of each reading and determine whether they entail contradictory conclusions about what the Mandate requires. Test whether a framework (e.g., ''international law of the period'') could coherently hold both premises.',
    'If foreclosed (logically impossible to hold both), the contest is a truth-value dispute requiring settlement by evidence or authority. If coexisting (both defensible from different frames), the contest is a frame-selection dispute where the outcome depends on whose interpretive authority prevails—which is the mandatory_interpretive_discretion reading. This is the meta-level uncertainty about the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether this reading logically forecloses its sibling dual_obligation_indigenous_rights').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_jnhp_tr_t0, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0, 0.12).
narrative_ontology:measurement(balfour_jnhp_tr_t5, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 5, 0.15).
narrative_ontology:measurement(balfour_jnhp_tr_t10, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 10, 0.18).
narrative_ontology:measurement(balfour_jnhp_tr_t15, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 15, 0.22).
narrative_ontology:measurement(balfour_jnhp_tr_t20, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 20, 0.25).
narrative_ontology:measurement(balfour_jnhp_tr_t25, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 25, 0.27).
narrative_ontology:measurement(balfour_jnhp_tr_t30, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(balfour_jnhp_be_t0, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 0, 0.61).
narrative_ontology:measurement(balfour_jnhp_be_t5, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 5, 0.67).
narrative_ontology:measurement(balfour_jnhp_be_t10, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(balfour_jnhp_be_t15, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(balfour_jnhp_be_t20, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(balfour_jnhp_be_t25, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(balfour_jnhp_be_t30, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 30, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(balfour_jnhp_su_t0, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(balfour_jnhp_su_t5, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(balfour_jnhp_su_t10, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(balfour_jnhp_su_t15, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(balfour_jnhp_su_t20, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(balfour_jnhp_su_t25, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(balfour_jnhp_su_t30, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 30, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__jewish_national_home_primacy, 0.18).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, mandate_system_league_oversight).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, zionist_settlement_land_acquisition).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, arab_national_movement_constraints).

% DUAL FORMULATION NOTE:
% The Balfour Mandate kernel decomposes into three structurally distinct constraint stories, each with different epsilon values and stakeholder structures. jewish_national_home_primacy (this story, high-extractive tangled_rope) interprets the Mandate as directed toward Jewish sovereignty through settlement facilitation and Arab political subordination. dual_obligation_indigenous_rights (sibling story, lower extractiveness, rope or scaffold) interprets the Mandate as requiring equal obligation to protect existing Arab rights. mandatory_interpretive_discretion (third sibling story, extraction mechanism) treats the British mandatory authority's interpretive power itself as the operative constraint. These three readings are not three perspectives on one constraint—they are three distinct constraints with different structural properties, different beneficiary/victim sets, and different epsilon values. They are linked via the ε-invariance decomposition principle: a single natural-language concept (the Mandate) that admits multiple structurally incompatible readings generates multiple constraint stories. Each story names what is at stake in the reading contest; the contest cannot be resolved by examining one story alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

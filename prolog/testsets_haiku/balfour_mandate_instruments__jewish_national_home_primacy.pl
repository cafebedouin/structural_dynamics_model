% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The Balfour Mandate (1920–1948) established a League of Nations
 *   trusteeship structure over Palestine with British administrative
 *   authority and a commitment to facilitate a 'Jewish national home.' This
 *   story instantiates ONE READING of a deeply contested kernel: the reading
 *   that interprets 'national home' as requiring demographic and
 *   institutional transformation toward Jewish sovereignty — proto-state
 *   apparatus, preferential immigration, facilitated land transfer, and
 *   structural subordination of Arab political institutions. The constraint
 *   operates as tangled_rope: it solves a coordination problem (organizing
 *   Jewish institutional development and immigration in a territory with
 *   competing claims) while asymmetrically extracting Palestinian Arab land,
 *   political authority, and territorial control. The Mandate text itself is
 *   ambiguous — sibling readings assert dual obligations to protect Arab
 *   rights, or British interpretive discretion as the primary constraint.
 *   This story generates the reading in which Jewish institutional primacy is
 *   the Mandate's governing interpretation.
 *
 * KEY AGENTS:
 *   - jewish_agency: quasi-governmental status under Article 4; negotiates directly with British mandatory; controls immigration certification and land-acquisition networks
 *   - jewish_migrants: gain preferential immigration access and settlement support; concentrated in urban and fertile areas; accumulate land assets and institutional capacity
 *   - zionist_institutions: build parallel state-like apparatus (Histadrut, Haganah, cooperative networks) with Mandate recognition; gain territory, capital, and military capacity
 *   - british_mandatory_authority: interprets Mandate dual obligations as mandating Jewish institutional advancement; facilitates land transfer, immigration quotas, and Jewish institutional autonomy
 *   - palestinian_arab_landholders: lose land through systematic transfer to Jewish entities; face economic pressure and administrative obstruction of reciprocal Arab purchases
 *   - palestinian_arab_political_leadership: structurally excluded from quasi-governmental status; subordinated to British mandatory's prioritization of Jewish institutional development
 *   - arab_laborers: lose land access and employment as Jewish institutional labor markets close and territory transfers
 *   - league_of_nations: holds formal oversight; exercises it only through post-hoc review; never reverses substantive British decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.81).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.79).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.81).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, 'fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66').
narrative_ontology:cs_kernel_codification('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', fixed_text).
narrative_ontology:cs_authority_grounding('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', lineage).
narrative_ontology:cs_interpretation_layer_present('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66').
narrative_ontology:cs_reading_relation('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_reading_relation('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', foundational, national_home_requires_demographic_majority).
narrative_ontology:cs_axiom_status(national_home_requires_demographic_majority, holdable).
narrative_ontology:cs_axiom_grounding('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', national_home_requires_demographic_majority, conventional).
narrative_ontology:cs_axiom('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', foundational, jewish_institutional_primacy_mandate_compatible).
narrative_ontology:cs_axiom_status(jewish_institutional_primacy_mandate_compatible, holdable).
narrative_ontology:cs_axiom_grounding('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', jewish_institutional_primacy_mandate_compatible, deontological).
narrative_ontology:cs_reference_frame('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', balfour_commitment_jewish_statehood_trajectory).
narrative_ontology:cs_drift_state('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', post_1935_mandate_enforcement_pivot, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd6d34aa-6519-4e82-8f6e-ca44f9ea1a66', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, international_zionist_movement).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, arab_laborers).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, mandate_system_legitimacy).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, great_power_self_determination_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains quasi-governmental status under Mandate Article 4, permitted to negotiate directly with the mandatory authority and to administer Jewish institutions (land acquisition, immigration processing, internal governance). Sets the terms for Jewish land purchase and settlement expansion. Controls the allocation of immigration certificates. Operates parallel institutional structures that gradually acquire state-like functions.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency, agenda_setter,
    institutional, generational, arbitrage, regional).

% Gain legal entry to Palestine under immigration quotas that prioritize Jewish entry; benefit from organized land-purchase networks that facilitate acquisition at preferential terms. Establish communities with institutional support, educational systems, and organized defense structures. Their demographic concentration transforms the territory's composition over two decades.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, mobile, global).

% Build the Histadrut (labor federation), Haganah (militia), cooperative settlement networks, and educational infrastructure with mandatory recognition and de facto exemption from Arab-administered governance. Accumulate capital, land, military capacity, and political authority as the constraint operates.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, arbitrage, regional).

% Holds formal administrative authority over Palestine under League of Nations Mandate. Interprets the Mandate's dual obligations (facilitate Jewish national home AND protect Arab civil/political rights) as a mandate to facilitate Jewish institutional development and land transfer while limiting Arab political institutions. Enforces immigration quotas, mediates land transactions, and maintains security arrangements that preserve Jewish institutional advantage.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority, agenda_setter,
    institutional, biographical, constrained, regional).

% Face systematic land purchase by Jewish entities at rising prices (extracting their land asset base). Restrictions on land sales within the constraint's framework are asymmetrically enforced: Jewish entities can acquire Arab land; reciprocal Arab acquisition of Jewish land is administratively obstructed. Economic pressure from displacement and demographic competition forces sales at terms increasingly unfavorable to Arab sellers.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    moderate, biographical, constrained, local).

% Structurally excluded from the quasi-governmental status granted to the Jewish Agency; denied equivalent institutional recognition and administrative capacity. Their political representation and negotiating power over Palestinian affairs are subordinated to the British mandatory's interpretation of the Mandate as prioritizing Jewish institutional development. They object to the constraint's operation but are locked out of the decision-making architecture it instantiates.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, excluded).

% Lose land access and employment as Jewish institutional structures establish sealed labor markets (Histadrut preferring Jewish workers) and as land transfers reduce Arab-controlled agricultural territory. Trapped within the region; unable to exit, unable to participate in the reorganized labor market, unable to block the constraint's operation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, arab_laborers, payer,
    powerless, biographical, trapped, local).

% Holds formal oversight authority over Mandate administration but exercises it only through post-hoc review of mandatory reports. The British mandatory controls interpretation and enforcement on the ground; the League's oversight is structurally attenuated and never reverses substantive British decisions during the Mandate period.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations, observer,
    institutional, generational, analytical, universal).

% Gains international legitimacy and institutional backing for the project of Jewish state formation; the Mandate structure channels organizational and financial resources toward Palestine specifically. The reading's interpretation of 'national home' as proto-state requirement validates their organizational strategy.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, international_zionist_movement, beneficiary,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, jewish_agency).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, administratively coherent authority (the British mandatory plus Jewish Agency) for organizing immigration, land tenure, and institutional development in Palestine — solving the coordination problem of bringing Jewish institutions into a territory with competing claims and existing Arab governance structures. Presents this as providing unified administrative oversight that prior Ottoman and localized Arab governance could not supply.
% TRANSFER_FUNCTION: Moves Palestinian Arab land from Arab to Jewish ownership through systematically facilitated purchase; transfers immigration slots and entry rights to Jewish applicants; channels administrative authority and quasi-governmental status to Jewish institutional structures (Jewish Agency, Histadrut, organized Zionist settlement networks) while restricting equivalent Arab institutional recognition and capacity. The constraint extracts Palestinian Arab political authority and territorial control.
% ABSENT_VOICES: Palestinian Arab landholders and political leadership who would object are structurally EXCLUDED from the Mandate's quasi-governmental architecture — the Jewish Agency holds a seat at the table that Arabs are denied. Their absence is not accidental; it is the constraint's enforcement mechanism. Arab nationalist movements and Arab League states would contest the reading's foundational premise (that 'national home' mandates demographic transformation favoring one group) but are outside the bilateral British-Zionist administrative frame.
% DISAPPEARANCE_RATIONALE: If the constraint and its enforcement machinery vanished — if the Jewish Agency lost quasi-governmental status, if immigration quotas opened equally to Arab and Jewish applicants, if land-transfer facilitation reversed to neutral brokerage, if Arab political representation equalized with Jewish institutional capacity — the project of establishing a Jewish-majority proto-state in Palestine would stall. The Mandate structure as the reading instantiates it is structurally necessary to the territorial and demographic transformation the reading claims to accomplish. Without it, Jewish institutional development and land accumulation proceed more slowly and face negotiated coexistence rather than demographic dominance.
% FOUNDING_PROBLEM: Prior to the Mandate, Jewish communities in Palestine lacked secure legal status, organized institutional capacity, and access to land and immigration on favorable terms. The Mandate was framed as solving this: providing legal recognition (Article 4 quasi-governmental status), guaranteed immigration access (Balfour commitment), and administrative facilitation for land acquisition and settlement. The 'national home' is interpreted as requiring proto-state infrastructure — not merely cultural community, but territorial control, demographic majority, and independent institutions.
% FOUNDING_PROBLEM_CORROBORATION: The British mandatory and Zionist leadership attest the founding problem is continuously live: lack of institutional recognition, insufficient immigration capacity, Arab resistance to land sales. Palestinian Arab leadership and later Arab League attests the founding problem is a constructed justification for territorial appropriation — that the 'founding problem' was manufactured to legitimize the displacement of an existing population with established land tenure and governance. Economic historians and colonial scholars (external to the benefiting parties) document that by the 1930s the 'founding problem' of legal status was solved, but the constraint persisted to facilitate demographic transformation beyond what institutional security required.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).

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
 *   Extractiveness (0.81 at endpoint) measures the Mandate's systematic transfer of land, political authority, and demographic dominance from Palestinian Arab hands to Jewish institutions. It rises over the 28-year interval as demographic transformation accelerates, land transfer compounds, and Jewish institutional capacity grows. Suppression (0.79) reflects the enforcement cost to prevent Arab exit and Arab institutional organization equivalent to the Jewish Agency's status — Arab political representation is administratively obstructed, Arab military organization is prohibited, land-sale barriers against Arab buyers are enforced asymmetrically. Theater (0.42) reflects the gap between security/administrative rationales offered for the constraint and the demographic-transformation function it actually serves: as the founding problem (Jewish institutional insecurity) diminishes, the constraint's enforcement intensifies precisely to maintain the demographic advantage, indicating the primary function has shifted from institutional security to territorial control. Accessibility collapse (0.62 at baseline, 0.85 at endpoint, structural level) rises as Arab alternatives close: they cannot acquire land freely, cannot immigrate to match Jewish demographic growth, cannot establish parallel institutions, cannot exit or reorganize the territory. The coercion grid shows differential pressure across levels: individual Arabs face land loss and employment exclusion (stakes_inflation 0.28→0.68); organizational Arab leadership faces institutional subordination and administrative obstruction (stakes_inflation 0.42→0.76); class-level Arab laborers and peasants face sealed labor markets and dispossession (resistance rises from 0.55→0.84 as they mount increasingly desperate opposition); structural level shows the Mandate frame itself privileges Jewish coordination and Arab subordination (suppression and resistance converge as the system hardens).
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish Agency and British mandatory perspective, the constraint solves the coordination problem of organizing Jewish immigration and settlement in a territory with competing claims; it provides security, institutional recognition, and administrative support. From this seat, the constraint is genuine rope-type coordination — the beneficiaries experience it as legitimate institutional development with security functions. From the Palestinian Arab landholding and political seats, the same structure operates as systematic extraction: land loss, political subordination, demographic displacement, and denial of equivalent institutional recognition. The engine computes per-seat directionality from the structural data (beneficiaries vs. victims + exit options + power atoms): Jewish seats with beneficiary role and mobile/arbitrage exit compute toward d=0.2–0.4 (beneficiaries with low extractiveness); Palestinian Arab seats with payer/victim role and trapped/identity_locked exit compute toward d=0.75–0.95 (targets with high extractiveness). The British mandatory authority, holding institutional power and constrained exit (they can reinterpret the Mandate but cannot simply withdraw), computes near d=0.5 (they administer the constraint but are also bound by its requirements). This per-seat divergence is structural, not opinion — it follows from the beneficiary/victim declarations and exit modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: jewish_agency (institutional status, administrative capacity), jewish_migrants (immigration access, settlement support), zionist_institutions (land accumulation, capital flow, military development). All three sit at the beneficiary end of directionality (d near 0.0–0.3) because the constraint's operation directly advances their interests and they retain mobile or arbitrage-grade exits (they could establish institutions elsewhere, though the Mandate concentration makes this suboptimal). Victims: palestinian_arab_landholders (systematic land loss, economic coercion, trapped in the territory watching their asset base disappear), palestinian_arab_political_leadership (institutional subordination, excluded from quasi-governmental status, identity-locked to the territory and unable to exit or reorganize it), arab_laborers (employment exclusion, land loss, trapped at the lowest power level with no alternatives). All three sit at the target end (d near 0.80–0.95) because the constraint extracts from them at compounding rates and their exit options are identity-locked or trapped — they cannot leave, cannot reorganize, cannot establish equivalent institutions. The british_mandatory_authority sits near symmetric (d≈0.45–0.55) because they administer the constraint and extract some bureaucratic benefit from it, but they are also structurally bound by the Mandate obligations and cannot unilaterally abandon either the Jewish facilitation or Arab protection without international repudiation. The League of Nations observer role sits at analytical d (removed from the beneficiary/victim distribution). This distribution drives the engine's type computation: high extraction beneficiary benefit (low d) + high extraction victim cost (high d) with active enforcement = tangled_rope signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mandate kernel distinguishes between the jewish_national_home_primacy reading (this story) and the dual_obligation_indigenous_rights sibling reading (equal or superior obligation to protect Arab civil/political rights) and the mandatory_interpretive_discretion sibling (British discretion is the primary constraint, not the Mandate text). Mandatrophy arises if the founding problem (Jewish institutional insecurity and lack of recognized status in Palestine) becomes obsolete while the constraint persists unchanged. The six-questions analysis shows this: founding_problem_status = 'contested' because Jewish legal status and institutional recognition are solved by 1930–1935 (the Jewish Agency is formally recognized, Jewish institutions operate autonomously, Jewish immigration is legally guaranteed), but the constraint intensifies 1935–1948 precisely to maintain demographic advantage beyond what institutional security requires. The founding_problem_corroboration notes that external parties (economic historians, colonial scholars, Arab League) attest the founding problem was solved and the constraint persists as structural extraction rather than institutional facilitation. This is a classic mandatrophy profile: the constraint was justified by a founding problem that was legitimately live (1920–1930), but persists into a regime (1935–1948) where the founding problem is dead and the constraint's primary function is territorial control rather than institutional security. The theater_ratio rise (0.25→0.42) documents this: the security/administrative rationale becomes increasingly theatrical as the enforcement machinery pivots from facilitating immigration/settlement to preventing Arab demographic resistance and maintaining Jewish demographic advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_dual_obligation_ambiguity,
    'Does the Mandate text impose dual obligations (facilitate Jewish national home AND protect Arab civil/political rights) at equal weight, or does it subordinate Arab protection to Jewish facilitation?',
    'Textual analysis (Article 2 vs. Article 4) and institutional history: did the mandatory authority treat the obligations as coequal, or did it weight one above the other in practice? What did contemporaneous League oversight communications assert about relative weight?',
    'If coequal, the constraint as authored is a false reading that overweights Jewish facilitation — a sibling dual_obligation_indigenous_rights reading becomes the structurally true constraint. If weighted unequally in the text itself, this reading''s interpretation is defensible from the source kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_text_dual_obligation_ambiguity, empirical, 'Textual and historical ambiguity: relative weight of dual Mandate obligations.').

omega_variable(
    jewish_agency_institutional_status_interpretation,
    'Does Mandate Article 4 grant the Jewish Agency genuine quasi-governmental status (administrative authority, law-making capacity, enforcement power over Jewish communities), or does it grant only consultative standing and representative capacity within structures the mandatory authority fully controls?',
    'Administrative history: what powers did the Jewish Agency actually exercise? Did the mandatory authority delegate administrative functions, or did it retain hierarchical control? What authority did Arab institutions hold by comparison?',
    'If Article 4 grants only consultative capacity, the extraction flow is differently channeled — the constraint becomes lower-extractiveness coordination with advisory asymmetry rather than quasi-governmental institution-building. If it grants genuine quasi-governmental authority, this reading''s institutional-dominance interpretation holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_agency_institutional_status_interpretation, empirical, 'Scope of Jewish Agency institutional authority under Article 4.').

omega_variable(
    reading_kernel_contest_structural_location,
    'Which reading of the Balfour Mandate kernel (jewish_national_home_primacy, dual_obligation_indigenous_rights, mandatory_interpretive_discretion) is the structurally dominant reading that actually governed British Mandate administration during 1920–1948?',
    'Administrative history: what interpretation did the British mandatory authority implement? What did the League of Nations oversight bodies hold when they reviewed Mandate compliance? What did the Zionist movement''s own communications claim they were extracting from the Mandate?',
    'This reading claims jewish_national_home_primacy dominated. If administrative practice reflects dual_obligation_indigenous_rights or mandatory_interpretive_discretion as the governing reading, the type and extraction profile changes — dual_obligation would lower extraction (shifting toward rope), mandatory_interpretive_discretion would reframe it as enforcement/suppression machinery rather than a specific beneficiary extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest_structural_location, conceptual, 'Which kernel reading was the structurally dominant interpretation in Mandate administration.').

omega_variable(
    palestinian_arab_suppression_internalization,
    'Is the measured suppression (0.79 at endpoint) a structural property (external barriers: legal exclusion, militia enforcement, economic coercion, administrative obstruction) or is part of it internalized (Palestinian Arab communities accepting the constraint as inevitable)?',
    'Post-constraint evidence: did Palestinian resistance and political formation persist after the Mandate ended with structural intensity? If suppression is substantially internalized, it should persist; if structural, it should drop when external enforcement withdraws.',
    'If substantially internalized, the constraint carries higher effective extractiveness than the structural measure suggests. If structural, the measured value is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_arab_suppression_internalization, empirical, 'Structural vs. internalized component of Palestinian Arab suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1920, 0.25).
narrative_ontology:measurement_basis(balf_tr_t1920, observed).
narrative_ontology:measurement(balf_tr_t1926, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1926, 0.29).
narrative_ontology:measurement_basis(balf_tr_t1926, observed).
narrative_ontology:measurement(balf_tr_t1932, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1932, 0.34).
narrative_ontology:measurement_basis(balf_tr_t1932, observed).
narrative_ontology:measurement(balf_tr_t1938, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1938, 0.39).
narrative_ontology:measurement_basis(balf_tr_t1938, observed).
narrative_ontology:measurement(balf_tr_t1943, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1943, 0.41).
narrative_ontology:measurement_basis(balf_tr_t1943, observed).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.42).
narrative_ontology:measurement_basis(balf_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement_basis(balf_be_t1920, observed).
narrative_ontology:measurement(balf_be_t1926, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1926, 0.68).
narrative_ontology:measurement_basis(balf_be_t1926, observed).
narrative_ontology:measurement(balf_be_t1932, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1932, 0.74).
narrative_ontology:measurement_basis(balf_be_t1932, observed).
narrative_ontology:measurement(balf_be_t1938, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1938, 0.79).
narrative_ontology:measurement_basis(balf_be_t1938, observed).
narrative_ontology:measurement(balf_be_t1943, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1943, 0.8).
narrative_ontology:measurement_basis(balf_be_t1943, observed).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.81).
narrative_ontology:measurement_basis(balf_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1920, 0.61).
narrative_ontology:measurement_basis(balf_su_t1920, observed).
narrative_ontology:measurement(balf_su_t1926, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1926, 0.66).
narrative_ontology:measurement_basis(balf_su_t1926, observed).
narrative_ontology:measurement(balf_su_t1932, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1932, 0.71).
narrative_ontology:measurement_basis(balf_su_t1932, observed).
narrative_ontology:measurement(balf_su_t1938, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1938, 0.75).
narrative_ontology:measurement_basis(balf_su_t1938, observed).
narrative_ontology:measurement(balf_su_t1943, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1943, 0.78).
narrative_ontology:measurement_basis(balf_su_t1943, observed).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.79).
narrative_ontology:measurement_basis(balf_su_t1948, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1920, tn=1948
narrative_ontology:measurement(balf_grid_01, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(class), 1920, 0.41).
narrative_ontology:measurement(balf_grid_02, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(class), 1948, 0.78).
narrative_ontology:measurement(balf_grid_03, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(individual), 1920, 0.35).
narrative_ontology:measurement(balf_grid_04, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(individual), 1948, 0.71).
narrative_ontology:measurement(balf_grid_05, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(organizational), 1920, 0.48).
narrative_ontology:measurement(balf_grid_06, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(organizational), 1948, 0.82).
narrative_ontology:measurement(balf_grid_07, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(structural), 1920, 0.55).
narrative_ontology:measurement(balf_grid_08, balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse(structural), 1948, 0.85).
narrative_ontology:measurement(balf_grid_09, balfour_mandate_instruments__jewish_national_home_primacy, resistance(class), 1920, 0.55).
narrative_ontology:measurement(balf_grid_10, balfour_mandate_instruments__jewish_national_home_primacy, resistance(class), 1948, 0.84).
narrative_ontology:measurement(balf_grid_11, balfour_mandate_instruments__jewish_national_home_primacy, resistance(individual), 1920, 0.48).
narrative_ontology:measurement(balf_grid_12, balfour_mandate_instruments__jewish_national_home_primacy, resistance(individual), 1948, 0.81).
narrative_ontology:measurement(balf_grid_13, balfour_mandate_instruments__jewish_national_home_primacy, resistance(organizational), 1920, 0.62).
narrative_ontology:measurement(balf_grid_14, balfour_mandate_instruments__jewish_national_home_primacy, resistance(organizational), 1948, 0.88).
narrative_ontology:measurement(balf_grid_15, balfour_mandate_instruments__jewish_national_home_primacy, resistance(structural), 1920, 0.58).
narrative_ontology:measurement(balf_grid_16, balfour_mandate_instruments__jewish_national_home_primacy, resistance(structural), 1948, 0.72).
narrative_ontology:measurement(balf_grid_17, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(class), 1920, 0.35).
narrative_ontology:measurement(balf_grid_18, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(class), 1948, 0.74).
narrative_ontology:measurement(balf_grid_19, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(individual), 1920, 0.28).
narrative_ontology:measurement(balf_grid_20, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(individual), 1948, 0.68).
narrative_ontology:measurement(balf_grid_21, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(organizational), 1920, 0.42).
narrative_ontology:measurement(balf_grid_22, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(organizational), 1948, 0.76).
narrative_ontology:measurement(balf_grid_23, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(structural), 1920, 0.48).
narrative_ontology:measurement(balf_grid_24, balfour_mandate_instruments__jewish_national_home_primacy, stakes_inflation(structural), 1948, 0.81).
narrative_ontology:measurement(balf_grid_25, balfour_mandate_instruments__jewish_national_home_primacy, suppression(class), 1920, 0.58).
narrative_ontology:measurement(balf_grid_26, balfour_mandate_instruments__jewish_national_home_primacy, suppression(class), 1948, 0.79).
narrative_ontology:measurement(balf_grid_27, balfour_mandate_instruments__jewish_national_home_primacy, suppression(individual), 1920, 0.52).
narrative_ontology:measurement(balf_grid_28, balfour_mandate_instruments__jewish_national_home_primacy, suppression(individual), 1948, 0.74).
narrative_ontology:measurement(balf_grid_29, balfour_mandate_instruments__jewish_national_home_primacy, suppression(organizational), 1920, 0.65).
narrative_ontology:measurement(balf_grid_30, balfour_mandate_instruments__jewish_national_home_primacy, suppression(organizational), 1948, 0.82).
narrative_ontology:measurement(balf_grid_31, balfour_mandate_instruments__jewish_national_home_primacy, suppression(structural), 1920, 0.61).
narrative_ontology:measurement(balf_grid_32, balfour_mandate_instruments__jewish_national_home_primacy, suppression(structural), 1948, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__jewish_national_home_primacy, 0.14).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% The Balfour Mandate kernel instantiates three structurally distinct constraints corresponding to three competing readings of 'national home' and the Mandate's dual obligations. jewish_national_home_primacy (this story) interprets the Mandate as directing demographic and institutional transformation favoring Jewish sovereignty. dual_obligation_indigenous_rights interprets equal or primary obligation to protect Arab civil/political rights. mandatory_interpretive_discretion treats British interpretive authority itself as the operative constraint. These three readings cannot coexist in a single constraint — they have incompatible epsilon values (Jewish-primacy reading is high-extractiveness tangled_rope; dual-obligation reading is lower-extractiveness rope with minority protections). The family is linked via affects_constraints to show they are interpretations of the same kernel document with competing legitimate readings. No reading is the 'true' constraint; the family structure itself is the fact: the Mandate's meaning is contested and the structural delta between readings is substantial.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, organized, 0.28).
constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

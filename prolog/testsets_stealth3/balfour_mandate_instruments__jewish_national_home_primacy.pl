% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [RESOLVED MANDATROPHY]
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
 *   human_readable: Mandate Instruments Operated Under the Jewish-National-Home-Primacy Reading
 *   domain: international law / colonial administration / state formation
 *
 * SUMMARY:
 *   Under this reading, the Mandate instruments operate as directives for
 *   demographic and territorial transformation in service of the pledged
 *   national home: the Jewish Agency holds recognized quasi-governmental
 *   standing (Article 4), immigration certificates are allocated to build the
 *   incoming community, land transfers from Arab to Jewish ownership are
 *   systematically facilitated through registries and legal frameworks, and
 *   Arab political representation remains structurally thinner than the
 *   Agency's despite the Arab population's majority. The arrangement also
 *   performs real administrative work — cadastral survey, public health,
 *   roads, an admissions pipeline — which is precisely what makes it hybrid
 *   rather than purely extractive. The colloquial label attached to these
 *   instruments covers multiple structurally distinct operational claims;
 *   this file authors only the one specified by its constraint_id, with the
 *   family decomposition recorded in the network note and kernel_context. KEY
 *   AGENTS (by structural relationship): - british_mandatory_government:
 *   Agenda setter ([institutional]/[mobile]) — administers and enforces;
 *   collects strategic positioning, pays fiscal-military and legitimacy costs
 *   - zionist_institutions: Primary beneficiary
 *   ([organized]/[identity_locked]) — receives institutional standing,
 *   certificate allocation, land-transfer facilitation -
 *   jewish_immigrant_settlers: Secondary beneficiary
 *   ([moderate]/[constrained]) — receives land, credit, and labor priority;
 *   exit doors closing behind them - palestinian_arab_land_tenants: Primary
 *   target ([powerless]/[trapped]) — bears eviction and displacement when
 *   proprietor sales complete - palestinian_arab_political_leadership: Target
 *   ([organized]/[constrained]) — representation downgraded; arrest,
 *   suspension, exile - palestinian_arab_wage_workers: Target
 *   ([powerless]/[trapped]) — excluded from expanding settlement-sector
 *   employment - binational_state_advocates: Excluded voice
 *   ([moderate]/[constrained]) — parity proposals given no institutional
 *   foothold - league_of_nations_mandates_commission: Analytical observer
 *   ([analytical]/[analytical]) — reviews reports and petitions; commands no
 *   enforcement
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.8).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.86).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.8).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Mandate Instruments Operated Under the Jewish-National-Home-Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international law / colonial administration / state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, 'ba10aa09-1bf2-4f81-87eb-c937df7a92b8').
narrative_ontology:cs_kernel_codification('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', fixed_text).
narrative_ontology:cs_authority_grounding('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', lineage).
narrative_ontology:cs_interpretation_layer_present('ba10aa09-1bf2-4f81-87eb-c937df7a92b8').
narrative_ontology:cs_reading_relation('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_reading_relation('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', foundational, national_home_pledge_binding_supreme_directive).
narrative_ontology:cs_axiom_status(national_home_pledge_binding_supreme_directive, holdable).
narrative_ontology:cs_axiom_grounding('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', national_home_pledge_binding_supreme_directive, conventional).
narrative_ontology:cs_axiom('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', secondary, state_building_requires_land_and_immigration_preconditions).
narrative_ontology:cs_axiom_status(state_building_requires_land_and_immigration_preconditions, holdable).
narrative_ontology:cs_axiom_grounding('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', state_building_requires_land_and_immigration_preconditions, instrumental).
narrative_ontology:cs_reference_frame('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', balfour_pledge_supreme_directive).
narrative_ontology:cs_drift_state('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', post_1939_white_paper, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ba10aa09-1bf2-4f81-87eb-c937df7a92b8', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrant_settlers).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_land_tenants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_wage_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Appointed by the League to administer Palestine under the Mandate text. Runs the departments: issues immigration certificates in agreed quotas, surveys and registers land titles, adjudicates transfer disputes, maintains police and military forces. Receives strategic value — a secured eastern Mediterranean position astride the Suez route — and pays for it with troops, subsidy arguments in London, and recurring crises. Exit is available in principle: relinquish the charge to a successor body, as eventually happens in 1947–48.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_government, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_government, beneficiary).

% The Zionist Commission, then the Jewish Agency, hold recognized representative status under Mandate Article 4: they sit with the administration, allocate immigration certificates among applicants, raise funds abroad, purchase land through the Jewish National Fund (held as inalienable property), and build schools, hospitals, and a labor economy. Their personnel staff quasi-official functions. Renouncing the home-building mission would dissolve the movement's purpose; the institutions cannot walk away from what they exist to do.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, identity_locked, continental).

% Arrive under certificate quotas; receive subsidized land, credit, and hiring priority inside the settlement sector. Roughly half a million come between the wars; many flee doors closing elsewhere — American quota walls, then Nazi Europe — which makes return or onward migration progressively unavailable. They absorb rising physical insecurity as Arab opposition escalates through the 1930s and into the terminal emergency.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrant_settlers, beneficiary,
    moderate, biographical, constrained, regional).

% Work plots they do not own. When large proprietors — often absentee owners resident in Beirut — sell valleys wholesale (Jezreel/Marj Ibn Amir), completion of sale triggers their removal. Cultivator-protection ordinances arrive in 1929–1936, apply narrowly, compensate partially, and enforce slowly. Some shift to hill villages or town fringes; few hold the capital or papers to leave the country. Petitions reach Jerusalem and Geneva but alter little on the ground.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_land_tenants, payer,
    powerless, biographical, trapped, local).

% Organizes petitions, delegations to London, boycotts of proposed legislative councils whose composition would ratify the demographic shift, and finally the general strike and revolt of 1936–39. Faces arrest, suspension of its committees (1937), exile of its chief figures, and standing offers of participation that its constituents read as legitimizing subordination. Representation remains structurally thinner than the Agency's despite commanding the larger population.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    organized, generational, constrained, national).

% Depend on seasonal citrus work and urban labor. Hebrew-labor campaigns reserve expanding Jewish-enterprise jobs for Jewish workers; displaced rural kin swell town unemployment. No union channel crosses the sectoral divide; wages sag as the labor pool grows.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_wage_workers, payer,
    powerless, immediate, trapped, regional).

% Intellectual circles (Brit Shalom and successors) propose parity arrangements — shared sovereignty, cantonal schemes — intended to preserve both communities' claims without a demographic race. Marginalized inside both national camps; publications go unread where it matters; no institutional foothold is ever granted. Would have objected forcefully in any forum that seriously weighed alternatives; none admits them.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, binational_state_advocates, excluded,
    moderate, biographical, constrained, regional).

% The Permanent Mandates Commission in Geneva reviews the mandatory's annual reports, receives Arab petitions from 1921 onward, and interrogates officials in session. Its minutes record every contested clause and every disputed count. It can recommend, question, and embarrass; it commands no enforcement.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_mandates_commission, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a functioning territorial administration where Ottoman structures dissolved: cadastral survey and title registration, public health and road systems, an immigration admissions pipeline with predictable quotas, and an official channel through which the internationally pledged national home is pursued. Solves the collective problem of administering a contested territory under League supervision.
% TRANSFER_FUNCTION: Moves land title from predominantly Arab ownership and tenancy to Jewish national institutions through brokered sales with mandatory facilitation; moves people via admission certificates allocated to the Agency; moves political standing by granting the Agency quasi-governmental advisory status while leaving the Arab majority without an equivalent recognized body; moves tax revenue collected generally into services distributed unevenly across the two communities.
% ABSENT_VOICES: The Arab peasantry is effectively voiceless in the forums where decisions are made — petitions travel upward but no peasant seat exists in London, Jerusalem's executive, or the Agency. Parity (binational-state) advocates hold coherent alternative designs and are admitted to no deciding body. Neighboring Arab governments enter the conversation only late and under duress. Municipal Arab notables willing to participate are outflanked by the nationalist leadership and distrusted by the administration alike.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the Yishuv's institutions, land registry, and immigrant stock without the legal-administrative scaffolding that allocates certificates, facilitates transfers, and confers official standing on the Agency; Arab political life would reorganize around restored majority representation; Britain would lose the pledged-commitment framework anchoring its presence and the strategic position it administers. Every named seat's situation depends on the arrangement continuing.
% FOUNDING_PROBLEM: Administering the post-Ottoman southern Levant while reconciling three overlapping and partly contradictory wartime commitments — to Arab independence (Husayn-McMahon correspondence), to Allied partition (Sykes-Picot), and to a Jewish national home (Balfour Declaration) — under newly invented League supervision.
% FOUNDING_PROBLEM_CORROBORATION: Contested along the same lines as the kernel itself. Corroborating sources outside the benefiting parties: Arab Executive petitions and testimony before the League's Permanent Mandates Commission (1921–1939) attest the protective clauses were not honored; the Peel Commission (1937) concluded the commitments were irreconcilable under the operative reading and recommended partition; the Anglo-American Committee of Inquiry (1946) reached parallel conclusions. Zionist institutions alone attest the founding problem as live and unfulfilled — the pledge still awaiting completion — which is precisely the beneficiary-seat reading this file instantiates.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Scores describe the primacy-operated arrangement at its 1948 end state, with all three tracked series on one shared eight-point grid (1920–1948; every tracked metric authored at every point — no substitution of scalars into missing rows). Extractiveness ends at 0.80: title moved from Arab ownership and tenancy to national institutions through facilitated sales, certificate allocation built one community preferentially, and representation stayed asymmetric throughout. The series climbs through the 1920s–30s, dips at 1939 (the White Paper's immigration cap and land-transfer restrictions throttled the program's throughput), resumes as wartime erosion opened loopholes, and peaks in the terminal breakdown. Theater_ratio 0.48: the cadastral, health, and public-works functions were real, but a rising share of official activity consisted of protective-clause procedure — cultivator-protection statutes invoked while displacement proceeded — defending legitimacy rather than delivering stated protections. Suppression requirement 0.86 tracks the enforcement burden's history: early gendarmerie deployments (0.40), a calm trough (1925), post-riot garrisons (1930), the three-year revolt's martial-law regime of collective punishment and demolition orders (0.78–0.83), a wartime dip, and terminal emergency. The oscillation is not noise: concession–repression alternation (Passfield restriction 1930 reversed by the MacDonald Letter 1931; revolt suppression followed by negotiated caps) taught both camps that escalation moves policy — intermittent reinforcement operating as part of the arrangement's mechanism, not beside it. After 1939 the suppression target rotates from Arab insurgents toward the previously favored community (intercepted immigration, hunted insurgents), which the series registers as sustained elevation rather than decay. Accessibility_collapse 0.55: parity and cantonal alternatives remained articulable but lost every institutional foothold. Resistance 0.80: petitions, delegations, boycotts, general strike, and armed revolt met the arrangement continuously across the whole interval.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structural data. At the payer seats, the instruments that deliver registry, health, and admission logistics operate as dispossession and representation-stripping — extraction experienced as the arrangement's substance, not its side effect. At the beneficiary seats the identical structure presents as pledge-fulfillment infrastructure: the Agency experiences its Article 4 standing as earned partnership in an internationally sanctioned project. The mandatory seat is internally split — strategic collection on one ledger, troop and legitimacy expenditure on another — and its eventual exit (relinquishing the charge to the United Nations) was priced as cheaper than continued enforcement. The excluded seat perceives a foreclosure of alternatives that the insider seats barely registered as options. Same-level differentiation: Arab political leadership and the Zionist institutions both hold organized power, yet their exits differ radically — the Agency's mission is constitutionally unthinkable to abandon (identity lock), while the Arab leadership's constraint is political (participation channels read as legitimizing subordination). The engine computes these divergences from power, exit, and directional data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: zionist_institutions (beneficiary, organized, identity_locked) sits near the beneficiary pole — the arrangement subsidizes its standing, intake, and land bank; identity lock amplifies stickiness, not extraction. jewish_immigrant_settlers (beneficiary, moderate, constrained) sit beneficiary-side, though closing exit doors behind them couples them tighter to the arrangement's fate. The three payer groups sit near the target pole: trapped tenants and wage workers at maximal target exposure; the organized leadership constrained but politically targeted. The mandatory government is the derivation-hard case: nominally an agenda-setting beneficiary, it nonetheless bears most enforcement costs — hence the explicit directionality override raising its d from a derived ~0.12 to 0.35, marking a near-symmetric position with real target-side cost exposure; the override is scoped to the institutional power atom, which in this story contains only that seat. Scope: the arrangement operates nationally (Palestine-wide verification burden) under global legitimation (League oversight); the engine scales effective extraction at the payer seats accordingly, while suppression remains unscaled raw structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The reconciliation mandate never performed: every major review (Hope Simpson 1930, French 1932, Peel 1937, Anglo-American Committee 1946) found the commitments irreconcilable as operated, yet the arrangement ran over a decade past the first such finding — outliving its function while retaining genuine administrative capacity. It was terminated by decision (British relinquishment, the UN partition recommendation), not by atrophy: successor administrations absorbed the working cadastre, health, and legal machinery, so almost none of the arrangement persists as performance. That profile — function-failed, enforcement-heavy, decisively wound down — is mandatrophy resolved rather than piton formation. The tangled_rope claim matters against mislabeling in both directions: a pure-extraction reading would erase the coordination the arrangement actually delivered (and which successors kept), while the arrangement's own pledge-fulfillment framing conceals the asymmetric transfer its operation performed. Keeping coordination function and extraction flow jointly visible is what the classification buys here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This file instantiates exactly one reading — jewish_national_home_primacy — of the contested kernel balfour_mandate_instruments. Classification is conditioned on that instantiation: which reading is treated as operative changes epsilon, the beneficiary/victim structure, and the computed type.',
    'Compare this file''s computed verdicts against the sibling files (dual_obligation_indigenous_rights, mandatory_interpretive_discretion) under the shared engine; the divergences locate the reading-dependence.',
    'Under the dual-obligation sibling the protective clauses become governing constraints: victim emphasis shifts to evicted tenants specifically, epsilon falls toward the middle band, and the type trends rope/scaffold. Under the discretion sibling the classified object is the adjudication apparatus itself, not the substantive program.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer routing: this story is one of three readings of the balfour_mandate_instruments kernel; classification is conditioned on reading choice.').

omega_variable(
    sibling_reading_structural_delta,
    'If the dual_obligation_indigenous_rights reading governed the same instruments, which structural elements flip?',
    'Instantiate the sibling file and diff the compiled beneficiary/victim arrays, per-seat chi, and computed type against this file.',
    'Land-transfer facilitation converts from a coordinated benefit stream into a regulated market gated by tenant protection; the Agency''s standing becomes consultative rather than quasi-governmental; Arab political representation upgrades to protected-minority guarantee.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta under the indigenous-rights sibling reading of the same kernel.').

omega_variable(
    disagreement_location_priority_ordering,
    'Where exactly do the three readings disagree? All accept that the instruments contain both a home-building directive and protective clauses for the existing population; the contest lies in the priority ordering between them and in the denotation of ''national home'' (proto-state trajectory versus cultural refuge).',
    'Drafting-history adjudication: Colonial Office records, Curzon''s annotated objections to the draft wording, and Permanent Mandates Commission exchanges establish what the framers took the clause ordering to be.',
    'Fixing the ordering determines whether the same clauses classify as coordination-dominant or extraction-dominant; the denotation question decides whether state-sovereignty preconditions fall inside or outside the pledge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_priority_ordering, conceptual, 'Disagreement is located in clause-priority ordering and ''national home'' denotation, not in the clauses'' existence.').

omega_variable(
    holder_self_assessment_vs_measured_flows,
    'The reading''s own holders assessed the arrangement as legitimate fulfillment of an international pledge (low extraction), while measured flows — completed evictions, certificate monopoly, representation asymmetry — register high extraction at the payer seats. Which index should epsilon carry?',
    'Reading-indexed epsilon convention (referent fixed to the standing arrangement; value authored by the reading''s lights) versus measured-flow accounting; cross-file comparison of sibling readings'' epsilon over the identical referent resolves the spread.',
    'Epsilon ranges from roughly 0.4 (holder self-assessment) to 0.8 (flow accounting) — straddling the coordination/extraction boundary and moving the computed type between rope and tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(holder_self_assessment_vs_measured_flows, conceptual, 'Epsilon indexing ambiguity: holder self-assessment versus measured transfer flows over the same referent.').

omega_variable(
    transjordan_separability_natural_experiment,
    'Are the arrangement''s administrative-coordination functions (cadastre, public health, roads, admissions processing) separable from the demographic-transformation program?',
    'Comparative administrative history of Transjordan — separated from the national-home program in 1921 under the same mandatory power — as a natural experiment: if comparable service delivery obtained without the transformation program, the functions are separable.',
    'Separable functions support treating the transformation layer as the extractive component riding on genuine coordination; inseparability would attribute part of the measured extraction to irreducible coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transjordan_separability_natural_experiment, empirical, 'Transjordan comparison tests coordination/extraction separability within the same mandatory administration.').

omega_variable(
    cultivator_protection_effectiveness,
    'Did the cultivator-protection legislation (ordinance 1929; acts 1933/1936) actually prevent tenant displacement, or did it operate as procedural cover while evictions proceeded?',
    'Cross-tabulate protection-statute invocation rates against land-court eviction records and the Hope Simpson (1930) and French (1932) reports'' counts of landless Arabs.',
    'Genuine protection would substantially lower payer-seat extraction and theater scores; a documented gap between statute and outcome raises theater_ratio and confirms the victim enumeration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultivator_protection_effectiveness, empirical, 'Effectiveness of protective statutes versus recorded displacement outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1920, 0.14).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1925, 0.19).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(balf_tr_t1933, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1933, 0.31).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1936, 0.37).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1939, 0.41).
narrative_ontology:measurement(balf_tr_t1944, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1944, 0.45).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.48).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1925, 0.56).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1930, 0.61).
narrative_ontology:measurement(balf_be_t1933, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1933, 0.68).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1936, 0.75).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1939, 0.64).
narrative_ontology:measurement(balf_be_t1944, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1944, 0.72).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1925, 0.36).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1930, 0.45).
narrative_ontology:measurement(balf_su_t1933, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1933, 0.52).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1936, 0.78).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1939, 0.83).
narrative_ontology:measurement(balf_su_t1944, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, resource_allocation).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label for the Mandate instruments conflates three structurally distinct constraints (epsilon-invariance decomposition). This file (jewish_national_home_primacy) authors the transformation-directive reading: epsilon 0.80, victims enumerated at the tenant/worker/leadership seats, type tangled_rope. dual_obligation_indigenous_rights authors the protective-clauses-govern reading over the same texts: lower epsilon, victim set narrowed to unprotected evictees, type trending rope/scaffold. mandatory_interpretive_discretion authors the adjudication-apparatus reading: its epsilon attaches to discretionary authority itself, upstream of both substantive readings. Upstream/downstream: the discretion reading mediates which substantive reading operates at any time; the primacy and dual-obligation readings cite the same clauses as evidence for opposite orderings. Every family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

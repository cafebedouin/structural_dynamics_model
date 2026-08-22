% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Retention Regime (Encyclopedic Museum Custody Doctrine)
 *   domain: international_law/cultural_property/post-colonial
 *
 * SUMMARY:
 *   The universal-heritage reading of the cultural-property corpus holds that
 *   artifacts of all civilizations form a common human inheritance whose
 *   legitimate custodians are the institutions best able to preserve and open
 *   them — in practice, the great encyclopedic museums of former imperial
 *   capitals. The standing arrangement under contest is the retention of
 *   colonial-era acquisitions, defended by treaty language, national museum
 *   law, immunity-from-seizure statutes, professional codes, and the 2002
 *   universal-museum declaration. CONSTRAINT FAMILY NOTE: the colloquial
 *   label 'cultural property law' covers three structurally distinct claims,
 *   authored as separate files per the epsilon-invariance principle. This
 *   file instantiates the universal-heritage reading and authors epsilon =
 *   0.74 for the retention arrangement as this reading assesses it: the
 *   reading endorses preservation-centered custody yet registers the real
 *   costs the arrangement imposes on claimant states (recurring legal and
 *   diplomatic expenditure, unpriced identity harm) and descendant
 *   communities. Under the sibling sovereign-repatriation reading, the same
 *   corpus reads as illegitimate holding of sovereign property with the
 *   holding institutions as targets; under the indigenous-stewardship
 *   reading, both states and museums appear as expropriators of communal
 *   title. Same corpus, different epsilon, different victim sets — hence
 *   separate stories linked by network edges, with the universal reading
 *   upstream (its 1954/1970 treaty language is cited against the downstream
 *   claimant readings).
 *
 * KEY AGENTS:
 *   - encyclopedic_museums: Agenda-setting custodian (institutional/identity_locked) — administers retention, collects revenue, prestige, and scholarly capital
 *   - holding_state_governments: Enforcing beneficiary (institutional/constrained) — supplies legal immunities, export controls, and treaty cover
 *   - post_colonial_claimant_states: Primary target (organized/trapped) — bears the legal, diplomatic, and political costs of pursuit
 *   - descendant_source_communities: Deepest target (powerless/trapped) — bears ceremonial exclusion and identity harm
 *   - global_museum_visiting_publics: Diffuse beneficiary (organized/mobile) — consumes access at subsidized prices
 *   - international_research_community: Secondary beneficiary (moderate/mobile) — consumes concentrated scholarly access
 *   - icprcp_mediation_body: Mediating observer (institutional/analytical) — recommends without binding force
 *   - indigenous_stewardship_advocates: Excluded voice (powerless/trapped) — communal-authority claims sit outside the frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.74).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.64).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Retention Regime (Encyclopedic Museum Custody Doctrine)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post-colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '6e5697f7-0320-49fb-afd9-d45ac2b04e3d').
narrative_ontology:cs_kernel_codification('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', formalized).
narrative_ontology:cs_authority_grounding('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', expertise).
narrative_ontology:cs_interpretation_layer_present('6e5697f7-0320-49fb-afd9-d45ac2b04e3d').
narrative_ontology:cs_reading_relation('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', cultural_property_legal_corpus__sovereign_repatriation_reading, forecloses).
narrative_ontology:cs_reading_relation('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', cultural_property_legal_corpus__indigenous_stewardship_reading, forecloses).
narrative_ontology:cs_axiom('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', foundational, preservation_maximization_confers_authority).
narrative_ontology:cs_axiom_status(preservation_maximization_confers_authority, holdable).
narrative_ontology:cs_axiom_grounding('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', preservation_maximization_confers_authority, instrumental).
narrative_ontology:cs_axiom('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', foundational, geographic_origin_irrelevant_to_custody).
narrative_ontology:cs_axiom_status(geographic_origin_irrelevant_to_custody, holdable).
narrative_ontology:cs_axiom_grounding('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', geographic_origin_irrelevant_to_custody, deontological).
narrative_ontology:cs_reference_frame('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', denationalized_common_patrimony).
narrative_ontology:cs_drift_state('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', contemporary_repatriation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6e5697f7-0320-49fb-afd9-d45ac2b04e3d', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, holding_state_governments).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, global_museum_visiting_publics).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, international_research_community).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, post_colonial_claimant_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, descendant_source_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, cultural_internationalism_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, common_heritage_of_mankind_principle).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_declaration_2002).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and display collections acquired largely during the colonial era; set acquisition, loan, and deaccession policy through trustee boards and professional codes; issued the 2002 Declaration affirming continued custody. Collect admission revenue, prestige, and scholarly capital from holding works of global significance. Voluntary return is legally barred in some jurisdictions by national museum acts and organizationally fraught everywhere; a handful of objects have gone back under narrow legislative exceptions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, agenda_setter,
    institutional, generational, identity_locked, global).

% Enact export controls, immunity-from-seizure statutes, and treaty positions that stabilize existing holdings; collect soft power, tourism income, and diplomatic capital from hosting the collections. Some have legislated narrow restitution pathways while keeping the broader framework intact; unwinding the framework wholesale would mean revising treaty positions, museum law, and domestic constituencies at once.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, holding_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, holding_state_governments, agenda_setter).

% Govern states whose patrimony was removed under colonial administration; pursue recovery through UNESCO's intergovernmental committee, bilateral negotiation, and occasional litigation. Every avenue runs through procedures the holding side staffs and funds; a claim carries years of diplomatic effort, legal expense, and domestic political cost, and most end in temporary loans or refusals rather than transfer of title. There is no forum outside the framework in which the claim can be brought.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, post_colonial_claimant_states, payer,
    organized, generational, trapped, continental).

% Communities with living cultural, religious, or ancestral continuity to held objects — royal courts, ritual specialists, descent lines. Objects central to ceremony or identity sit in foreign storerooms; physical access requires travel, visas, and institutional permission granted case by case. Their claims reach the process only after passing through state channels they do not control.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, descendant_source_communities, payer,
    powerless, generational, trapped, regional).

% See works from many civilizations gathered in single visits, often at admission prices subsidized by holding states. Their access depends on reaching a small number of northern capitals; they bear none of the arrangement's costs and form its largest constituency.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, global_museum_visiting_publics, beneficiary,
    organized, biographical, mobile, global).

% Scholars and conservators gain concentrated access to cross-cultural collections, comparative datasets, and laboratory infrastructure housed with the holdings. Research careers and agendas are built around that access; public criticism of retention practices can carry professional cost within the field.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_research_community, beneficiary,
    moderate, biographical, mobile, global).

% UNESCO's Intergovernmental Committee for Promoting the Return of Cultural Property convenes both sides, mediates individual disputes, and issues recommendations with no binding force. Its effectiveness depends entirely on member-state cooperation and the goodwill of holding institutions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, icprcp_mediation_body, observer,
    institutional, generational, analytical, global).

% Communities and organizations asserting direct communal authority over sacred or communally held items appear in the process mainly as petitioners to state- and institution-run procedures, never as principals. Domestic frameworks recognizing communal stewardship exist in a few countries but sit outside the international frame in which custody questions are decided.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_stewardship_advocates, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a distributed network of conserved, catalogued, and publicly accessible collections: conservation science, standardized documentation, loan exchange, and wartime-protection planning under the 1954 Hague framework are provided once, centrally, rather than rebuilt by each nation separately.
% TRANSFER_FUNCTION: Moves custodial control, admission revenue, prestige, and scholarly capital from claimant states and descendant communities to holding institutions and their host cities; moves the costs of recovery — legal fees, diplomatic effort, decades of negotiation — onto claimant states.
% ABSENT_VOICES: Descendant communities and communal-stewardship advocates are heard only as petitioners inside state- and institution-run processes; claimant-state publics are represented by foreign ministries rather than present themselves; joint committees exist for a handful of famous objects, not the wider corpus. The unanimity of the 2002 universal-museum declaration reflected the signatories' shared position — the parties whose claims it forecloses were not among the signatories.
% DISAPPEARANCE_RATIONALE: Museum economies, international loan networks, conservation funding streams, and national patrimony programs all presuppose the retention framework. Overnight removal would trigger mass claims, emergency legislation, renegotiated loan regimes, and a scramble over who conserves what — the arrangement's absence would force a new settlement among states, institutions, and communities, not leave a vacuum.
% FOUNDING_PROBLEM: War and imperial conquest had scattered and destroyed cultural property for centuries; mid-century states built a protection framework, culminating in the 1954 Hague Convention, so that the works of all peoples would survive armed conflict and be safeguarded rather than taken as spoils.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO bodies and Hague-convention state parties attest that the wartime-protection problem remains live. The Sarr-Savoy report commissioned by the French presidency, claimant-state cultural ministries, and independent provenance researchers attest that peacetime retention of colonial-era acquisitions is no longer justified by that founding problem — corroboration from outside the benefiting parties supports the shifted-function reading.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because retention is decoupled from any continuing service entitlement: a claimant state pays anew for every claim — counsel, diplomacy, negotiation cycles — while title essentially never moves, and the identity harm to descendant communities is unpriced anywhere in the arrangement. Suppression (0.64) is predominantly structural: statutes barring deaccession, immunity-from-seizure legislation, export controls, and treaty implementation gaps; it is short of total because legislative exceptions have produced real returns, so the barrier is high but permeable. Theater ratio (0.42) reflects a growing performative share alongside genuinely functional conservation and scholarship: token long-term loans presented as resolution, 'shared heritage' rhetoric deployed against the very claims it deflects, universality proclaimed while access concentrates in a few northern capitals. Accessibility collapse is moderate (0.45) because alternatives remain partly workable — bilateral agreements, legislative restitution paths, long-term loan frameworks, digital surrogates — so understanding the arrangement does not eliminate every exit. Resistance is substantial (0.65): sustained state campaigns, UN General Assembly resolutions, the ICOM definitional shift, and high-profile returns. The three measurement series run on one shared seven-point grid (1954–2026) so every metric is authored at every examined time point; the trajectories show enforcement machinery maturing (rising suppression_requirement) in step with accumulating extraction, with reform pulses (post-2018 returns) visible as slope changes rather than reversals.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is the guardianship performed daily — conservation laboratories, catalogues, millions of visitors, loans crossing borders; from the payer seats the same galleries read as a vault where their history is stored behind other people's ticket booths. The engine computes per-seat classifications from the structural data; the divergence between the identity-locked institutional seat, into which the gains accrue, and the trapped payer seats is the measurable gap. Identity-lock dynamics: the encyclopedic museum's exit is blocked less by law than by institutional self-concept — the universal mission is constitutive of what the institution believes it is, so returning core collections would dissolve the organizational identity, not merely shrink it; a museum that reframed itself as steward-until-return would compute very differently. Same-level dynamics: claimant states differ widely in raw power (Egypt, Greece, China versus smaller states), yet all meet the same procedural wall — power varies, exit does not — which is why the payer seat computes uniformly despite heterogeneous members. Suppression here is overwhelmingly structural (legal and treaty barriers); a minor internalized component runs through professional socialization in the research community, where access-dependence discourages criticism.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-d end: encyclopedic_museums collect the arrangement's gains directly and administer its rules; holding_state_governments collect soft power and tourism while supplying enforcement; global_museum_visiting_publics and international_research_community consume access with mobile exit and no cost exposure. Targets cluster at the high-d end: post_colonial_claimant_states bear recurring pursuit costs with no forum outside the framework (trapped), and descendant_source_communities bear identity and ceremonial harm with no independent channel (trapped, powerless). The beneficiary/victim declarations map cleanly onto these real relationships, so no directionality overrides are authored — and none could be safely authored anyway, since overrides key to power atoms that several distinct seats share. Suppression is authored as a raw structural property and is deliberately NOT scaled; only extractiveness is scaled by the engine, by directionality and spatial scope — the regime's global scope amplifies effective extraction on trapped targets because verification and remedy require multilateral process the holding side staffs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both symmetrical errors. Calling this arrangement a rope would launder retention as neutral coordination — the declared beneficiary/victim structure forbids it, since the same network that conserves also blocks. Calling it a snare would erase the real goods the network delivers (conservation science, documentation, wartime-protection obligations, genuine if asymmetric access) and would wrongly predict total collapse if enforcement lifted, when in fact the standards and documentation layers would likely survive a title transfer. The tangled-rope reading keeps both facts load-bearing: genuine coordination function, asymmetric extraction through the same structure, active enforcement required to hold it. On mandate: the founding problem (wartime protection of endangered works) remains live for its original object, but it no longer justifies the arrangement's now-primary function — peacetime legitimation of retention — which is why founding_problem_status is authored contested rather than dead, and why the drift omega routes the question to evidence rather than assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'This constraint instantiates the universal_heritage_reading of the cultural_property_legal_corpus kernel; would instantiating a sibling reading change the structural classification?',
    'Author the sibling files (sovereign_repatriation_reading, indigenous_stewardship_reading) and compare computed per-seat classifications across the family.',
    'Under the sovereign reading, holding institutions become targets and successor states beneficiaries — directionality inverts and the incidence of effective extraction shifts; under the stewardship reading, both states and museums appear as expropriators of communal title and the victim set widens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer structure: this is one of three readings of the cultural-property kernel; the disagreement is located in who holds legitimate authority over contested artifacts.').

omega_variable(
    preservation_access_separability,
    'Is the conservation-and-access function separable from the retention function, or does preservation genuinely require centralized custody in the holding institutions?',
    'Outcome tracking of transferred objects (Benin bronzes to Nigeria, recent Smithsonian and German returns): conservation condition, access levels, and scholarly output before and after transfer.',
    'If the functions are separable, the measured extraction exceeds coordination cost by the full retention premium and the arrangement trends toward pure extraction; if inseparable, part of the measured extraction is the price of the good itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_access_separability, empirical, 'Whether the coordination and retention components of the arrangement are structurally separable.').

omega_variable(
    universal_access_symmetry,
    'Is ''universal access'' substantively symmetric, or concentrated among those able to reach a few northern capitals?',
    'Visitor-origin statistics, loan-fee and insurance-cost data, and travel-plus-visa cost analysis weighed against source-region incomes.',
    'Demonstrably asymmetric access collapses the coordination half of the arrangement toward cover and pushes classification toward pure extraction; genuinely broad access sustains the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_access_symmetry, empirical, 'Whether the access good is distributed as universally as the doctrine claims.').

omega_variable(
    colonial_acquisition_consent_status,
    'Were colonial-era acquisitions consensual transfers under then-operative law, or takings under conditions that made consent structurally impossible?',
    'Archival provenance reconstruction, transaction-record analysis, and comparison against contemporaneous legal standards for valid transfer.',
    'Systematic non-consent converts retained objects from legitimately pooled goods into disputed title, raising effective extraction and strengthening the claimant-side readings of the same corpus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_acquisition_consent_status, empirical, 'Whether the original acquisitions underlying the retention regime were consensual.').

omega_variable(
    founding_function_drift,
    'Has the arrangement''s operative function drifted from wartime protection of endangered works to peacetime legitimation of retention?',
    'Compare doctrinal development and enforcement activity aimed at wartime safeguarding versus retention defense across the interval; the suppression_requirement series here tracks the latter.',
    'Confirmed drift dates mandate obsolescence for the retention function and supports renegotiation-or-sunset remedies; refuted drift supports the reading''s own account of itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_function_drift, conceptual, 'Whether the founding function still describes what the arrangement primarily does.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 1954, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpl_uhr_tr_t1954, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(cpl_uhr_tr_t1970, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(cpl_uhr_tr_t1985, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(cpl_uhr_tr_t1998, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1998, 0.3).
narrative_ontology:measurement(cpl_uhr_tr_t2010, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(cpl_uhr_tr_t2018, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(cpl_uhr_tr_t2026, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(cpl_uhr_be_t1954, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1954, 0.45).
narrative_ontology:measurement(cpl_uhr_be_t1970, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(cpl_uhr_be_t1985, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(cpl_uhr_be_t1998, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1998, 0.63).
narrative_ontology:measurement(cpl_uhr_be_t2010, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(cpl_uhr_be_t2018, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2018, 0.71).
narrative_ontology:measurement(cpl_uhr_be_t2026, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2026, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(cpl_uhr_su_t1954, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement(cpl_uhr_su_t1970, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(cpl_uhr_su_t1985, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(cpl_uhr_su_t1998, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(cpl_uhr_su_t2010, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(cpl_uhr_su_t2018, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement(cpl_uhr_su_t2026, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the cultural_property_legal_corpus kernel. The colloquial label 'cultural property law' conflates three structurally distinct claims: this file (universal_heritage_reading, epsilon 0.74, holding institutions as beneficiaries, claimant states as payers), sovereign_repatriation_reading (epsilon authored for the same corpus as illegitimate holding of sovereign property — holding institutions become targets), and indigenous_stewardship_reading (both states and museums appear as expropriators of communal title). The universal reading is upstream: its 1954 Hague and 1970 UNESCO treaty language is cited as authoritative against the downstream claimant readings, so contamination propagates from this file toward its siblings. Each member carries a single stable epsilon over the fixed referent (the standing retention arrangement); the differences are reading-indexed values over a shared referent, not measurement noise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

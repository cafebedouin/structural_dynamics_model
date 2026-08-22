% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Constitutional Authority
 *   domain: political/constitutional theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the constitutional_text kernel:
 *   the popular_sovereignty_reading, under which the written text's authority
 *   flows from the constituent power of the demos and neither courts nor
 *   legislatures hold conclusive interpretive finality. As an operative
 *   structure, the doctrine binds institutional actors — courts must issue
 *   rulings under a standing popular reservation, legislatures cannot settle
 *   constitutional questions by ordinary statute, and the professional
 *   interpretive class is ranked below mobilized popular will. Its operation
 *   is episodic: dormant for decades, then decisive at constituent surges
 *   (post-civil-war reconstruction amendments, progressive-era revision
 *   waves, twentieth-century executive mobilizations against courts, and the
 *   recent global wave of constituent processes). The colloquial label
 *   'constitutional authority' decomposes, per the epsilon-invariance
 *   principle, into three structurally distinct claims — judicial finality,
 *   parliamentary finality, and reserved popular ultimacy — each with its own
 *   epsilon, beneficiary/victim structure, and classification. This file
 *   authors epsilon for the standing popular-reservation arrangement as this
 *   reading assesses it: a real but episodic transfer of decision-rights away
 *   from institutions, with the referent fixed on the arrangement under
 *   contest, never on the rival readings' arrangements. KEY AGENTS (by
 *   structural relationship): - mobilized_citizen_majorities: Primary
 *   beneficiary (organized/constrained) — exercises the reserved channels and
 *   receives the decision-rights transferred at operative moments -
 *   plebiscitary_political_leaders: Strategic beneficiary
 *   (powerful/arbitrage) — invokes the sovereign people against institutional
 *   checks when advantageous - constitutional_courts: Primary target
 *   (institutional/trapped) — denied final interpretive authority; cannot
 *   exit the text whose ownership is contested - national_legislatures:
 *   Target (institutional/trapped) — ordinary legislation cannot settle
 *   constitutional questions - legal_expertise_community: Secondary target
 *   (organized/constrained) — professional interpretive authority
 *   subordinated at collision points - outvoted_minorities: Diffuse target
 *   (powerless/trapped) — bound by constituent outcomes without consent -
 *   disfranchised_residents: Excluded voice (powerless/trapped) — bound by
 *   processes in which they cannot vote -
 *   comparative_constitutional_scholars: Analytical observer — sees the full
 *   three-reading structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.42).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.32).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Popular Sovereignty Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "political/constitutional theory").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'de912449-e153-4041-be18-4582dd552d4d').
narrative_ontology:cs_kernel_codification('de912449-e153-4041-be18-4582dd552d4d', fixed_text).
narrative_ontology:cs_authority_grounding('de912449-e153-4041-be18-4582dd552d4d', practice).
narrative_ontology:cs_reading_relation('de912449-e153-4041-be18-4582dd552d4d', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('de912449-e153-4041-be18-4582dd552d4d', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('de912449-e153-4041-be18-4582dd552d4d', foundational, ultimate_interpretive_authority_reserved_to_demos).
narrative_ontology:cs_axiom_status(ultimate_interpretive_authority_reserved_to_demos, holdable).
narrative_ontology:cs_axiom_grounding('de912449-e153-4041-be18-4582dd552d4d', ultimate_interpretive_authority_reserved_to_demos, deontological).
narrative_ontology:cs_axiom('de912449-e153-4041-be18-4582dd552d4d', secondary, institutional_interpretive_finality_is_usurpation).
narrative_ontology:cs_axiom_status(institutional_interpretive_finality_is_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('de912449-e153-4041-be18-4582dd552d4d', institutional_interpretive_finality_is_usurpation, deontological).
narrative_ontology:cs_reference_frame('de912449-e153-4041-be18-4582dd552d4d', constituent_power_of_the_demos).
narrative_ontology:cs_drift_state('de912449-e153-4041-be18-4582dd552d4d', contemporary_amendment_freeze, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('de912449-e153-4041-be18-4582dd552d4d', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, mobilized_citizen_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, grassroots_constitutional_campaigns).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, plebiscitary_political_leaders).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, national_legislatures).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legal_expertise_community).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, outvoted_minorities).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, popular_sovereignty_principle).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, right_of_revolution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens who organize at scale to revise constitutional fundamentals — amendment drives, constituent assembly petitions, mass mobilization around charter questions. When they succeed, the settlement they win becomes the new baseline that courts and legislatures must apply. Their leverage runs through the extraordinary channels the doctrine reserves to them; stepping back returns them to ordinary voter status with no special standing.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, mobilized_citizen_majorities, beneficiary,
    organized, generational, constrained, national).

% Campaign committees and civic associations that gather signatures, run initiative petitions, and lobby for convention calls. They convert diffuse dissatisfaction into formal revision proposals. A campaign that fails can redirect its networks and donor bases to other causes; the personnel outlast any single effort.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, grassroots_constitutional_campaigns, beneficiary,
    moderate, biographical, mobile, national).

% Executives and party leaders who invoke the sovereign people against institutional checks — casting courts as unelected elites and insisting electoral mandates outrank procedural limits. The invocation is a resource they deploy when it serves them and shelve when it does not; their office gives them reach no social movement enjoys.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, plebiscitary_political_leaders, beneficiary,
    powerful, biographical, arbitrage, national).

% The highest courts exercising judicial review. Every ruling they issue is issued under a standing reservation that the people can overturn it by amendment or disregard it in a constituent moment; the finality they claim is precisely what the doctrine denies. They cannot resign the role — their authority exists inside the very text whose ownership is contested — and their prestige depends on presenting themselves as servants of a popular will they cannot conclusively bind.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_courts, payer,
    institutional, generational, trapped, national).

% Elected legislative bodies. Ordinary statutes cannot settle constitutional questions under this doctrine; supermajority amendment rules and the standing possibility of a convention hang over everything they produce. They cannot leave the arrangement — they are constituted by the same text — and their work is perpetually exposed to reversal through channels they do not control.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, national_legislatures, payer,
    institutional, generational, trapped, national).

% Jurists, constitutional law faculties, bar associations, and professional commentators whose craft is authoritative interpretation. The doctrine ranks their reasoned judgments below mobilized popular will whenever the two collide, and careers built on interpretive authority lose ground in constituent moments. Some individuals migrate into politics or advocacy; the professional identity itself keeps most inside the system they criticize.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legal_expertise_community, payer,
    organized, generational, constrained, national).

% Groups on the losing side when constituent power moves — religious minorities, regional populations, dissenters swept up in supermajority revision. Amendment and convention outcomes bind them without their consent; emigration is costly and citizenship is not opt-out. Between constituent moments they depend for protection on the very institutions the doctrine subordinates.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, outvoted_minorities, payer,
    powerless, generational, trapped, national).

% Adults subject to the constitution who cannot vote in the referenda, ratifications, or elections through which 'the people' acts — non-citizens, territorial residents, and others barred from the franchise. Every settlement reached in their name binds them, yet they hold no seat in the process that claims to speak for the whole.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, disfranchised_residents, excluded,
    powerless, generational, trapped, national).

% Academic observers tracking how constitutional systems allocate final interpretive authority — documenting constituent episodes from Philadelphia to Reykjavik to Santiago and theorizing where ultimate authority should sit. They bear neither the costs nor the gains of any particular allocation; their analyses travel across jurisdictions.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, mobilized_citizen_majorities).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the legitimacy and fundamental-change problem of a written constitution: it gives the order a peaceful meta-channel for revising its own foundations (amendment, convention), aligns obedience to law with continuing popular consent across generations, and prevents any single institution from locking in its own interpretation of the text permanently.
% TRANSFER_FUNCTION: Moves final decision-rights over constitutional meaning away from sitting institutions (courts, legislatures) and reserves them for the demos acting through extraordinary channels; at operative moments it moves concrete policy outcomes toward mobilized majority preferences and away from institutional or expert settlements.
% ABSENT_VOICES: Outvoted minorities at constituent moments, disfranchised residents, and future generations bound by today's revisions are not seated in the conversation; they would object that 'the people' as invoked is never the whole people. Adherents of the rival readings participate in public discourse but are structurally absent from constituent processes dominated by mobilized majorities.
% DISAPPEARANCE_RATIONALE: If the popular reservation vanished overnight — if constitutional meaning were conclusively fixed by courts or legislatures with no reserved popular override — amendment politics, movement strategy, and legitimacy narratives would all rearrange. Institutions would face no standing appeal beyond themselves; campaign infrastructure built around initiative and convention channels would dissolve or repurpose; and the recurring pattern by which mobilized publics reset constitutional baselines would simply stop occurring.
% FOUNDING_PROBLEM: Monarchical and parliamentary authority had claimed to derive from divine right or institutional tradition, letting rulers self-authorize. The arrangement was built to ground constitutional obligation in a source superior to any governing institution — so that no king, court, or parliament could become the final judge of its own powers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: constitutional courts themselves — the seats that bear this doctrine's costs — routinely acknowledge the people as the ultimate source of constitutional authority in preamble invocations and ratification-era jurisprudence; comparative constitutional scholarship (Sieyès, Locke's revolutionary tradition, Ackerman's constitutional moments, the Latin American constituent-process literature) documents the problem and the reserved-power answer across unrelated traditions; and defenders of judicial finality typically concede the amendment channel as the people's reserved authority, implicitly attesting that the founding problem — institutional self-authorization — remains the thing to guard against.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 (end-of-interval standing level): when the doctrine operates it transfers something substantial — conclusive decision-rights — but its operation is episodic, institutions retain day-to-day function, and the transfer runs to a diffuse popular seat rather than a concentrated capturer. Suppression is authored low-moderate at 0.32: the rival readings are not suppressed (accessibility_collapse 0.25 — they remain fully live positions), and the doctrine's coercive edge (the reserved right of revolution, majoritarian override risk) is latent rather than continuously applied. Suppression is a raw structural property left unscaled; only extractiveness is scaled by directionality and spatial scope in the engine's computation. Theater_ratio is high at 0.58 because in mature democracies the doctrine's observable activity is overwhelmingly rhetorical — preamble invocations, mandate talk, civic ritual — while operative episodes are rare. High theater here does NOT indicate an inertial-performance profile: the distinction from atrophy is that identifiable beneficiaries actively maintain and revive the function (campaign infrastructure, convention movements, recurring constituent processes worldwide), which is documented in the dormancy_vs_atrophy omega and the mandatrophy analysis. Resistance is high at 0.62: courts assert finality through practice, legislatures harden amendment rules, and the expertise community supplies the counter-majoritarian scholarship that contests the doctrine — institutional friction is the doctrine's principal obstacle. The temporal series run on one shared grid (t = 0, 40, 80, 120, 160, 200, 240) and show a cyclical pattern rather than monotonic drift: extractiveness and enforcement-capacity spike at mobilization surges (reconstruction-era overrides, progressive-era amendments, mid-interval executive mobilizations) and decay between them. The oscillation is the mechanism operating, not intermittent reinforcement — the doctrine works precisely by surging. The suppression_requirement series deliberately traces enforcement-capacity change: the credible-mobilization machinery that backs the reservation builds at surges and decays across the long amendment freeze of the late interval. Scalar base_properties values are measured at interval end (t=240, a quiescent phase). Coalition note: outvoted_minorities are individually powerless, and coalition potential exists (minority-rights organizations joining amendment campaigns), but historically constituent surges have outmaneuvered such coalitions; the engine should read their seat as weakly coordinated at best. Claim/metric independence: claimed_type tangled_rope is asserted from structure — a genuine coordination function (legitimacy plus a peaceful fundamental-change channel), asymmetric costs (institutions, expertise, and outvoted minorities pay), and episodic but real active enforcement — while the metrics are authored independently from the doctrine's observed operation; where the engine's computed type diverges from the claim, that divergence is data.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute sharply different types from identical structural data. From the constitutional_courts and national_legislatures seats, the doctrine is a delegitimating imposition: it strips their claims to finality while leaving them administratively indispensable — a structure they experience as extraction from institutional autonomy. From the mobilized_citizen_majorities and grassroots_constitutional_campaigns seats, the same structure is a reserved guarantee: standing permission to override, experienced as empowerment. The legal_expertise_community seat is doubly positioned — it invokes popular legitimacy in its own scholarship while being the doctrine's most articulate critic. At operative moments the outvoted_minorities seat computes a third thing again: majoritarian override wearing the mantle of the whole people. Constitutional courts sit in a distinctive reflexive position — they are targets of the doctrine yet constantly cite it to legitimate their own rulings ('we the people' framings), which is why their derived directionality is high despite their rhetorical embrace of the principle. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. mobilized_citizen_majorities and grassroots_constitutional_campaigns sit near the beneficiary end (d low): the reserved channels subsidize their activity and their constrained exit (demobilization returns them to ordinary politics) keeps them invested. plebiscitary_political_leaders derive low d amplified by arbitrage-grade exit — they collect the doctrine's benefits selectively and bear almost none of its costs, making them the seat nearest pure subsidy. constitutional_courts and national_legislatures are trapped payers (d near the target end): they cannot exit the text, their time horizon is generational, and the doctrine takes from them precisely the good they are constituted to hold (finality). legal_expertise_community derives moderately high d — constrained rather than trapped, since individual members can migrate into politics or advocacy, but the profession as a whole cannot. outvoted_minorities combine the weakest power atom with trapped exit at national scope, pushing their effective extraction toward the maximum the formula produces: they bear binding outcomes with no exit and no seat. National spatial scope applies modest verification-difficulty amplification across all seats — constitutional meaning is exactly the kind of claim whose observables are contested at scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing any governing institution from becoming the final judge of its own powers — remains live: institutions still seek self-aggrandizement, and the question of who guards constitutional meaning recurs in every jurisdiction. The R5 mismatch consumer therefore reads status=live paired with verdict=world_rearranges: no zombie flag, and correctly so — the arrangement's persistence tracks a persisting problem, not a dead mandate. The mandatrophy apparatus earns its keep here in both directions. First, it blocks a mislabeling of the doctrine as pure extraction: the costs to courts, legislatures, expertise, and minorities are real, but they ride on a genuine coordination function (intergenerational consent and a peaceful fundamental-change channel) without which the constitutional order could not legitimately revise itself at all. Second, it blocks the opposite error — reading the doctrine's high theater_ratio and late-interval extractiveness decline as terminal atrophy. The correct diagnosis is episodic dormancy: a fire-extinguisher profile, not a rusted one. The dormancy_vs_atrophy omega makes that diagnosis falsifiable, and the fixing_cost assessment (prohibitive) records why no seat can cheaply retire the reservation: any institutional attempt to abolish it would trigger the very mobilization it reserves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation_delta,
    'This story instantiates the popular_sovereignty_reading of the constitutional_text kernel; how would the sibling readings (judicial_supremacy_reading, legislative_sovereignty_reading) restructure the beneficiary/victim sets and epsilon?',
    'Compare the three sibling stories'' computed per-seat classifications and epsilon values; the disagreement is located in the locus of conclusive interpretive authority — court, parliament, or extra-institutional demos.',
    'Under judicial supremacy, courts move to the benefiting and agenda-setting side and popular mobilization becomes the suppressed alternative; under legislative sovereignty, the sitting legislature captures final say and the override channel replaces constituent mobilization. Effective extraction flips sign across seats depending on which reading is instantiated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation_delta, conceptual, 'Committer structure: this constraint is one of three readings of the constitutional_text kernel; sibling readings redistribute who benefits and who pays.').

omega_variable(
    demos_identification_ambiguity,
    'When the demos exercises its reserved authority, who legitimately speaks for it — referendum pluralities, convention delegates, mobilized street majorities, or plebiscitary executives claiming mandates?',
    'Comparative analysis of constituent episodes: who convened, who voted, and whose exclusion invalidated the claim (Iceland 2011, Chile 2021, Kenya 2010, Bolivia 2009).',
    'If plebiscitary executives or bare pluralities count as ''the people,'' the doctrine''s costs concentrate on outvoted minorities at operative moments and its extraction profile sharpens; if only inclusive supermajoritarian channels count, costs stay diffuse and the institutional targets dominate the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_identification_ambiguity, conceptual, 'Whether the beneficiary seat ''the demos'' resolves to inclusive or exclusive operational definitions.').

omega_variable(
    dormancy_vs_atrophy,
    'Is the doctrine''s long quiescence in mature democracies the normal dormancy of an episodic mechanism, or atrophy of a function no longer performed?',
    'Track constituent-process frequency across established democracies over coming decades; a full generation with zero successful extra-institutional revisions in any mature democracy would indicate atrophy.',
    'Atrophy would push the classification toward an inertial, performance-maintained profile despite the authored tangled_rope claim; recurring constituent revivals confirm the episodic-operation reading and the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_vs_atrophy, empirical, 'Distinguishes fire-extinguisher dormancy from genuine functional death.').

omega_variable(
    revolution_clause_operativity,
    'Does the reserved right of revolution carry operative content in consolidated constitutional states, or is it purely symbolic residue?',
    'Test against crisis episodes: whether the theoretical reservation ever modulates institutional behavior short of actual revolt — executive mobilizations against courts, institutional compliance with adverse constituent demands, deliberate softening of amendment barriers under pressure.',
    'A purely symbolic revolution clause lowers the doctrine''s credible enforcement ceiling and its suppression component; an operative reservation raises both and widens the gap between the payer seats'' exposure and the beneficiary seats'' security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolution_clause_operativity, conceptual, 'Operative versus symbolic status of the doctrine''s most extreme reserved channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(cons_tr_t80, constitutional_text__popular_sovereignty_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement(cons_tr_t120, constitutional_text__popular_sovereignty_reading, theater_ratio, 120, 0.4).
narrative_ontology:measurement(cons_tr_t160, constitutional_text__popular_sovereignty_reading, theater_ratio, 160, 0.47).
narrative_ontology:measurement(cons_tr_t200, constitutional_text__popular_sovereignty_reading, theater_ratio, 200, 0.54).
narrative_ontology:measurement(cons_tr_t240, constitutional_text__popular_sovereignty_reading, theater_ratio, 240, 0.58).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(cons_be_t80, constitutional_text__popular_sovereignty_reading, base_extractiveness, 80, 0.53).
narrative_ontology:measurement(cons_be_t120, constitutional_text__popular_sovereignty_reading, base_extractiveness, 120, 0.56).
narrative_ontology:measurement(cons_be_t160, constitutional_text__popular_sovereignty_reading, base_extractiveness, 160, 0.51).
narrative_ontology:measurement(cons_be_t200, constitutional_text__popular_sovereignty_reading, base_extractiveness, 200, 0.46).
narrative_ontology:measurement(cons_be_t240, constitutional_text__popular_sovereignty_reading, base_extractiveness, 240, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(cons_su_t80, constitutional_text__popular_sovereignty_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(cons_su_t120, constitutional_text__popular_sovereignty_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(cons_su_t160, constitutional_text__popular_sovereignty_reading, suppression_requirement, 160, 0.46).
narrative_ontology:measurement(cons_su_t200, constitutional_text__popular_sovereignty_reading, suppression_requirement, 200, 0.36).
narrative_ontology:measurement(cons_su_t240, constitutional_text__popular_sovereignty_reading, suppression_requirement, 240, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional authority' conflates three structurally distinct claims about the locus of conclusive interpretive authority. Per the epsilon-invariance principle, decomposed into a three-story constraint family: judicial_supremacy_reading (epsilon authored for the court-finality arrangement), legislative_sovereignty_reading (epsilon authored for the parliamentary-finality arrangement), and this file (epsilon authored for the reserved-popular-ultimacy arrangement). Each member carries its own epsilon, beneficiary/victim structure, and classification; the upstream/downstream influence between them runs through which seat each reading empowers — whichever reading dominates a jurisdiction's practice changes the legitimacy conditions and resource availability for the other two. This story links to both siblings; each sibling should link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Reification of Jati Categories
 *   domain: social anthropology / religious studies / political economy
 *
 * SUMMARY:
 *   A colonial and post-colonial administrative apparatus stabilized jati
 *   (caste/community) categories into a fixed, ranked, enumerable schedule:
 *   the decennial census from 1871, the gazetteers and ethnographic surveys
 *   built on it, the constitutional schedules that inherited it in 1950, and
 *   the certificate-verified quota system that administers it today. Beneath
 *   and before the register, communities named themselves variously across
 *   context, village, and decade; the register made one printed answer
 *   consequential everywhere at once. This story is the
 *   colonial_census_reading of the jati_practice_norm kernel and decomposes
 *   the colloquial label 'the caste system' into three structurally distinct
 *   constraints per the epsilon-invariance principle: this reading authors
 *   epsilon 0.66 against the standing enumeration arrangement; the
 *   orthodox_textual_reading authors its own epsilon against a
 *   scripture-enforced hierarchy whose victims are ritual deviants; the
 *   localized_practice_reading authors near-coordination-cost epsilon against
 *   fluid local norms. The siblings are separate files linked through
 *   network.affects_constraints. Relation choices: this reading coexists_with
 *   the textual reading (administrators and pandits held both, sometimes
 *   fused in one framework); it influences the localized reading (the freeze
 *   changed the operating environment of local boundary-making, routing
 *   renegotiation through petitions against the printed list, without
 *   eliminating local practice). Claim and metrics are authored
 *   independently: the claim (tangled_rope) states the structure this reading
 *   believes true — a real coordination function carrying asymmetric
 *   extraction — while the metrics describe the arrangement's actual
 *   operation. KEY AGENTS (by structural relationship): -
 *   colonial_census_bureaucracy: agenda-setter and principal beneficiary
 *   (institutional/arbitrage) — designs the schedule, collects the legibility
 *   rents - district_revenue_and_police_officers: secondary beneficiary
 *   (institutional/mobile) — consumes the frozen map for revenue,
 *   recruitment, policing - brahmin_textual_intermediaries: beneficiary
 *   (organized/constrained) — supplies scriptural rankings, gains
 *   administrative sanction - native_landholding_elites: beneficiary
 *   (powerful/constrained) — gains legible tenantry and certified precedence
 *   - status_mobile_middle_castes: primary target among the mobile
 *   (organized/trapped) — fluid status frozen mid-hierarchy -
 *   rank_stigmatized_lower_castes: primary target (powerless/trapped) —
 *   bottom ranks administratively certified -
 *   scheduled_and_backward_class_members: post-1947 dual-positioned
 *   target-beneficiary (organized/identity_locked) -
 *   caste_based_political_parties: post-Mandal beneficiary
 *   (institutional/mobile) — harvests the frozen categories electorally -
 *   successor_state_welfare_ministries: post-1947 agenda-setter
 *   (institutional/arbitrage) — administers the inherited schedule -
 *   anti_caste_reform_movements: excluded voice (organized/constrained) —
 *   objected from outside the administrative conversation -
 *   postcolonial_scholarly_commentariat: analytical observer
 *   (moderate/analytical) — documents the construction thesis
 *
 * KEY AGENTS:
 *   - colonial_census_bureaucracy: agenda-setter and principal beneficiary (institutional/arbitrage) — designs the decennial schedule, publishes the tables, collects the legibility rents
 *   - district_revenue_and_police_officers: secondary beneficiary (institutional/mobile) — consumes the frozen map for revenue assessment, labor and military quotas, policing
 *   - brahmin_textual_intermediaries: beneficiary (organized/constrained) — supplies scriptural precedence orders that acquire state imprimatur
 *   - native_landholding_elites: beneficiary (powerful/constrained) — princes and zamindars gaining legible tenantry and printed confirmation of precedence
 *   - status_mobile_middle_castes: primary target among the mobile (organized/trapped) — trading, scribal, and agricultural communities whose rising status claims froze mid-hierarchy
 *   - rank_stigmatized_lower_castes: primary target (powerless/trapped) — communities whose bottom-rank placement became certified public fact
 *   - scheduled_and_backward_class_members: post-1947 dual-positioned target-beneficiary (organized/identity_locked) — livelihood and stigma both run through the listed category
 *   - caste_based_political_parties: post-Mandal beneficiary (institutional/mobile) — build coalitions on the frozen category map
 *   - successor_state_welfare_ministries: post-1947 agenda-setter (institutional/arbitrage) — administer the inherited schedule as constitutional law
 *   - anti_caste_reform_movements: excluded voice (organized/constrained) — Satyashodhak, Arya Samaj, Gandhian, and Ambedkarite currents objecting from outside the enumeration's councils
 *   - postcolonial_scholarly_commentariat: analytical observer (moderate/analytical) — historians and anthropologists documenting the construction thesis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.66).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.6).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Reification of Jati Categories").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social anthropology / religious studies / political economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '0533af29-9ab9-4ee6-ba6b-8cdded192bee').
narrative_ontology:cs_kernel_codification('0533af29-9ab9-4ee6-ba6b-8cdded192bee', formalized).
narrative_ontology:cs_authority_grounding('0533af29-9ab9-4ee6-ba6b-8cdded192bee', extraction).
narrative_ontology:cs_interpretation_layer_present('0533af29-9ab9-4ee6-ba6b-8cdded192bee').
narrative_ontology:cs_reading_relation('0533af29-9ab9-4ee6-ba6b-8cdded192bee', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('0533af29-9ab9-4ee6-ba6b-8cdded192bee', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('0533af29-9ab9-4ee6-ba6b-8cdded192bee', foundational, enumeration_constitutes_category).
narrative_ontology:cs_axiom_status(enumeration_constitutes_category, holdable).
narrative_ontology:cs_axiom_grounding('0533af29-9ab9-4ee6-ba6b-8cdded192bee', enumeration_constitutes_category, conventional).
narrative_ontology:cs_axiom('0533af29-9ab9-4ee6-ba6b-8cdded192bee', foundational, legibility_requires_fixity).
narrative_ontology:cs_axiom_status(legibility_requires_fixity, holdable).
narrative_ontology:cs_axiom_grounding('0533af29-9ab9-4ee6-ba6b-8cdded192bee', legibility_requires_fixity, instrumental).
narrative_ontology:cs_reference_frame('0533af29-9ab9-4ee6-ba6b-8cdded192bee', enumerated_jati_schedule).
narrative_ontology:cs_drift_state('0533af29-9ab9-4ee6-ba6b-8cdded192bee', post_independence_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0533af29-9ab9-4ee6-ba6b-8cdded192bee', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_census_bureaucracy).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, district_revenue_and_police_officers).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, brahmin_textual_intermediaries).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, native_landholding_elites).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, caste_based_political_parties).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, successor_state_welfare_ministries).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, status_mobile_middle_castes).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, rank_stigmatized_lower_castes).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, scheduled_and_backward_class_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, scheduled_and_backward_class_members).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, colonial_legibility_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, martial_race_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the decennial questionnaire, fixes the category list and its ordering rules, trains enumerators, and publishes the tables and gazetteers that become the official social map. District administrations, recruiting boards, and later welfare ministries consume its schedules. Its officers advance by producing ever finer compilations; exit is a posting elsewhere in the imperial or successor service, never life inside a listed category.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_census_bureaucracy, agenda_setter,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, colonial_census_bureaucracy, beneficiary).

% Assess land revenue, fill labor and military quotas, and police vagrancy and so-called criminal tribes by looking up answers in the published tables rather than negotiating with each locality. They gain ready-made answers at low cost; a transfer moves them to another district with the same tables waiting.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, district_revenue_and_police_officers, beneficiary,
    institutional, biographical, mobile, regional).

% Serve as paid informants and ranking authorities, supplying scriptural precedence orders that the census adopts and prints. Their traditional status hierarchy acquires state imprimatur and a recurring consulting income; their position depends on staying useful to the enumeration, and their learned authority travels poorly outside the census relationship.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, brahmin_textual_intermediaries, beneficiary,
    organized, generational, constrained, regional).

% Princes, zamindars, and large landlords receive legible tenantry rolls, certifiable service populations, and printed confirmation of their own precedence. They host census operations and lobby over their own rankings; their standing rests on the agrarian order the tables describe, so they work within it rather than against it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, native_landholding_elites, beneficiary,
    powerful, generational, constrained, regional).

% Trading, scribal, and agricultural communities whose status claims were rising through patronage, army service, and wealth when the tables fixed them at intermediate ranks. They pour resources into petition campaigns, caste associations, and litigation to move their printed entry; leaving the category system is not available to them — every school, regiment, and revenue roll reads the same entry.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, status_mobile_middle_castes, payer,
    organized, biographical, trapped, regional).

% Communities printed at the bottom of published hierarchies, their separateness now a matter of official record cited in schools, barracks, and villages alike. Untouchability practices harden behind the printed table. Individually they lack the standing to contest entries; collectively they eventually build electoral weight, but no exit from the list exists — only struggle over its terms.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, rank_stigmatized_lower_castes, payer,
    powerless, generational, trapped, national).

% After independence, members of listed communities receive reserved seats, school places, jobs, and credit through certificate-verified category membership. The same listing follows them in every interaction with the state; concealing or renouncing it forfeits the claims, and neighbors, employers, and marriage markets read the label regardless. Their relationship to the list is permanent and two-sided: livelihood runs through it, and so does the stigma.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, scheduled_and_backward_class_members, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, scheduled_and_backward_class_members, beneficiary).

% Build voter coalitions by promising list placement, sub-quotas, and protection to named communities. They gain a ready-made mobilization map at negligible organizing cost and defend the list's salience in every election; they can rebrand between cycles and are not themselves listed.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, caste_based_political_parties, beneficiary,
    institutional, immediate, mobile, national).

% Inherit the colonial tables as constitutional schedules, then administer certificates, quota compliance, and inclusion adjudication. Every program they run is keyed to the list; simplifying or dissolving it would break delivery overnight, while maintaining it yields a governable, targetable population. They sit above the categories they administer.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, successor_state_welfare_ministries, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, successor_state_welfare_ministries, beneficiary).

% Satyashodhak, Arya Samaj, Gandhian, and Ambedkarite currents denounce the printed hierarchy and the ritual order behind it. They stand outside the enumeration's councils — their objections enter as protest and pamphlet, not as questionnaire design — though after 1947 Ambedkarite leadership writes the schedules' constitutional form, turning outside objection into inside administration.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, anti_caste_reform_movements, excluded,
    organized, biographical, constrained, national).

% Historians and anthropologists document how the enumeration reshaped the categories it claimed to describe, publishing critiques that travel through universities and policy seminars. They bear none of the list's costs and collect none of its rents; their exit is the library.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, postcolonial_scholarly_commentariat, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, successor_state_welfare_ministries).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the imperial information problem: a thin foreign administration governing hundreds of millions needed a standardized social map — who lived where, in what numbers, under what hereditary occupational and status labels — for taxation, famine-relief targeting, military recruitment, epidemic control, and electoral design. It also coordinated the state's own agents: district officers, settlement departments, and recruiting boards acted on one shared category vocabulary instead of thousands of incompatible local self-descriptions.
% TRANSFER_FUNCTION: Moves classification authority upward from communities to the administrative center: communities lost the power to name themselves and their neighbors in any officially consequential way. It moves precedence downward as published fact: census-ranked hierarchies certified status claims and slurs with state imprimatur. After 1947 it moves distributive goods — legislative seats, university places, government jobs — through the frozen category slots, from the general pool to listed communities.
% ABSENT_VOICES: The enumerated communities themselves — especially those whose self-understanding did not fit the questionnaire's hereditary-occupation box; women, described by male household heads; nomadic and forest-dwelling groups, who were criminalized under the Criminal Tribes Act rather than consulted. Their objections surfaced only as enumeration refusals, petition campaigns, and anomalous returns, which administrators treated as data-quality problems rather than claims about the instrument.
% DISAPPEARANCE_RATIONALE: Constitutional schedules, central and state reservation quotas, caste-based parties, certificate-verified welfare delivery, and the entire post-Mandal political alignment are load-bearing on the frozen category list. Overnight deletion would void constitutional schedules, strand millions of entitlement claims, dissolve party coalitions, and force wholesale reconstruction of welfare targeting — a decade-scale rearrangement.
% FOUNDING_PROBLEM: Imperial governance legibility: a small British cadre ruling a vast, internally differentiated subcontinent lacked any reliable map of its social composition — who could be taxed, conscripted, recruited as martial races, vaccinated, or represented. A secondary founding motive was orientalist scholarship: compiling the peoples of India as an ethnographic encyclopedia.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by the anti-caste reform press of the period (the Satyashodhak critique of census-ranked hierarchy) and, decisively, by post-colonial historiography independent of any administering state — the colonial-knowledge studies of Bernard Cohn and Nicholas Dirks's Castes of Mind document that the founding problem was imperial legibility and that it lapsed with the Raj in 1947. The administering parties, colonial and successor alike, narrate an unbroken neutral-statistics mission instead; no beneficiary seat corroborates the dead-status finding, which is itself the signal.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.66 for the standing arrangement — the enumeration regime from the 1871 census through present-day schedule administration — assessed by this reading's own lights: an external apparatus took over the fixing of community boundaries, published ranked hierarchies with state imprimatur, and now allocates livelihoods through the frozen slots. Suppression (0.60) is authored as a raw structural property, unscaled by power or scope: compulsory enumeration backed by penal provisions, police-assisted counting, the adjacent criminalization of nomadic groups, and today's certificate-verification machinery. Theater (0.33) reflects a real informational product wrapped in a layer of speculative ethnography — cephalic indexes, racial theory, precedence tables of doubtful validity — that performed scholarship while driving policy. Accessibility collapse (0.55): once the register speaks, official-context alternatives close (one's consequential identity is the printed entry), yet household and local practice retained latitude the register never fully reached. Resistance (0.50): enumeration boycotts, petition wars over rankings, anti-caste mobilization, and contemporary sub-classification litigation. The measurement series shares one twelve-point grid (1871-2021); every tracked metric is authored at every point. The series rises to the 1931 peak (last full caste tabulation), troughs across the wartime-and-transition years (1941 simplified tables; 1951 general caste count dropped), then re-hardens as certificate enforcement matures — a regime transition, not an oscillation, so no cyclical-reinforcement reading applies. The base_properties scalars report the end-state (2021).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the census bureaucracy's chair the arrangement is a coordination device it built and runs — legible maps, cheap answers, targetable programs. From the rank-stigmatized seat the same tables are certified stigma with no exit. The status-mobile seat experiences a contest over a fixed object it cannot leave. The post-1947 listed-member seat holds both sides at once: livelihood through the list, stigma through the list, identity fused to the list. Successor ministries experience maintenance burden and delivery capability together. The engine derives these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (census bureaucracy, district officers, brahmin intermediaries, landholding elites, caste parties, successor ministries) derive low directionality — the arrangement subsidizes them, and arbitrage-grade or mobile exit pushes them further toward the beneficiary end. Declared victims (status-mobile middle castes, rank-stigmatized lower castes, listed members) derive high directionality, amplified by trapped and identity-locked exits. The listed-member seat is dual-positioned (payer with beneficiary secondary role): targeted goods pull its derived d downward, identity lock pushes it upward; the engine weighs the structural declarations. Continental and national spatial scopes modestly amplify effective extraction for the target seats — verifying category boundaries across a subcontinent is hard, and the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — imperial governance legibility — died with the Raj in 1947; the arrangement persisted by repurposing (constitutional schedules, welfare targeting, electoral mapping). The R5 interview records founding_problem_status dead against disappearance_verdict world_rearranges: the mismatch that flags repurposed capture for investigation. The receipt surface names the successor ministries as the seat the gains land on, and fixing is prohibitive (constitutional entanglement, entitlement dependence, party-system load-bearing). The classification discipline cuts both ways: reading the arrangement as pure extraction erases the genuine coordination it performs (a shared social map, targeted redress); reading it as benign coordination erases the expropriated naming authority and the certified stigma. The tangled-rope claim keeps both faces in view, and the temporal series preserves the extraction trough and re-hardening that a pure-type story would smooth away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_of_boundary_authority,
    'Within the jati_practice_norm kernel, where does boundary-fixing authority reside — the state''s register (this reading), scriptural varna doctrine (orthodox_textual_reading), or continuous local negotiation (localized_practice_reading)?',
    'Comparative compilation of the three sibling constraint stories; the engine''s per-reading classifications locate the disagreement structurally rather than resolving it doctrinally.',
    'Adopting a sibling reading changes the victim set (textual deviants vs. enumerated communities vs. no stable victim set), the enforcement mode (ritual vs. administrative vs. customary), and epsilon itself; this story''s classification holds only under the census reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_boundary_authority, conceptual, 'Committer-frame omega: this constraint is one reading of kernel jati_practice_norm; sibling readings relocate boundary authority and restructure victims and enforcement.').

omega_variable(
    construction_versus_recording_degree,
    'Did the census apparatus substantially create bounded, ranked jati categories where earlier social forms were fluid, fuzzy, and context-varying, or did it mainly record boundaries that were already comparatively firm?',
    'Pre-1871 vernacular sources, itinerant-community self-descriptions, marriage-network studies, and comparison of local records against census categories (the Cohn-Dirks research program and its critics).',
    'Higher constructedness raises epsilon (more naming autonomy expropriated by the freeze) and strengthens the tangled-rope reading; lower constructedness lowers epsilon and pulls this reading toward convergence with the localized_practice_reading''s picture plus an administrative overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(construction_versus_recording_degree, empirical, 'Degree to which the enumeration froze fluid categories versus recorded already-firm ones.').

omega_variable(
    post_colonial_persistence_function,
    'Does the post-1947 persistence of the frozen schedule serve compensatory coordination (targeted redress lowering net extraction) or category-entrenching rent distribution (raising it)?',
    'Counterfactual evaluation of individualized versus category-based welfare delivery; tracking whether sub-classification contests and the certificate economy grow or shrink the autonomy cost over time.',
    'If compensatory coordination dominates, the modern segment trends rope-ward and the late-series rise reflects enforcement of entitlements rather than extraction; if entrenchment dominates, the arrangement trends snare-ward and the capture signal hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_persistence_function, empirical, 'Whether the schedule''s afterlife is redistributive coordination or entrenched rent allocation.').

omega_variable(
    stigmatized_seat_coalition_conversion,
    'Can the rank-stigmatized, formally powerless seat convert diffuse suffering into coalition power (as Ambedkarite mobilization in fact did), and does that conversion raise resistance enough to alter the arrangement''s equilibrium?',
    'Historical tracking of Dalit political formation, temple-entry and tank-entry satyagrahas, and the electoral weight of Scheduled Caste vote blocs.',
    'Successful conversion raises the resistance metric over time and forces the administering seats to bargain, softening unopposed extraction; failure leaves the seat trapped and the extraction uncontested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stigmatized_seat_coalition_conversion, empirical, 'Coalition-power potential of the most stigmatized victim seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1871, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1871, jati_practice_norm__colonial_census_reading, theater_ratio, 1871, 0.18).
narrative_ontology:measurement(jati_tr_t1881, jati_practice_norm__colonial_census_reading, theater_ratio, 1881, 0.24).
narrative_ontology:measurement(jati_tr_t1901, jati_practice_norm__colonial_census_reading, theater_ratio, 1901, 0.34).
narrative_ontology:measurement(jati_tr_t1911, jati_practice_norm__colonial_census_reading, theater_ratio, 1911, 0.36).
narrative_ontology:measurement(jati_tr_t1921, jati_practice_norm__colonial_census_reading, theater_ratio, 1921, 0.38).
narrative_ontology:measurement(jati_tr_t1931, jati_practice_norm__colonial_census_reading, theater_ratio, 1931, 0.4).
narrative_ontology:measurement(jati_tr_t1941, jati_practice_norm__colonial_census_reading, theater_ratio, 1941, 0.35).
narrative_ontology:measurement(jati_tr_t1951, jati_practice_norm__colonial_census_reading, theater_ratio, 1951, 0.28).
narrative_ontology:measurement(jati_tr_t1971, jati_practice_norm__colonial_census_reading, theater_ratio, 1971, 0.25).
narrative_ontology:measurement(jati_tr_t1991, jati_practice_norm__colonial_census_reading, theater_ratio, 1991, 0.27).
narrative_ontology:measurement(jati_tr_t2011, jati_practice_norm__colonial_census_reading, theater_ratio, 2011, 0.31).
narrative_ontology:measurement(jati_tr_t2021, jati_practice_norm__colonial_census_reading, theater_ratio, 2021, 0.33).

% Extraction over time
narrative_ontology:measurement(jati_be_t1871, jati_practice_norm__colonial_census_reading, base_extractiveness, 1871, 0.44).
narrative_ontology:measurement(jati_be_t1881, jati_practice_norm__colonial_census_reading, base_extractiveness, 1881, 0.5).
narrative_ontology:measurement(jati_be_t1901, jati_practice_norm__colonial_census_reading, base_extractiveness, 1901, 0.58).
narrative_ontology:measurement(jati_be_t1911, jati_practice_norm__colonial_census_reading, base_extractiveness, 1911, 0.61).
narrative_ontology:measurement(jati_be_t1921, jati_practice_norm__colonial_census_reading, base_extractiveness, 1921, 0.63).
narrative_ontology:measurement(jati_be_t1931, jati_practice_norm__colonial_census_reading, base_extractiveness, 1931, 0.65).
narrative_ontology:measurement(jati_be_t1941, jati_practice_norm__colonial_census_reading, base_extractiveness, 1941, 0.57).
narrative_ontology:measurement(jati_be_t1951, jati_practice_norm__colonial_census_reading, base_extractiveness, 1951, 0.53).
narrative_ontology:measurement(jati_be_t1971, jati_practice_norm__colonial_census_reading, base_extractiveness, 1971, 0.56).
narrative_ontology:measurement(jati_be_t1991, jati_practice_norm__colonial_census_reading, base_extractiveness, 1991, 0.61).
narrative_ontology:measurement(jati_be_t2011, jati_practice_norm__colonial_census_reading, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement(jati_be_t2021, jati_practice_norm__colonial_census_reading, base_extractiveness, 2021, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1871, jati_practice_norm__colonial_census_reading, suppression_requirement, 1871, 0.42).
narrative_ontology:measurement(jati_su_t1881, jati_practice_norm__colonial_census_reading, suppression_requirement, 1881, 0.48).
narrative_ontology:measurement(jati_su_t1901, jati_practice_norm__colonial_census_reading, suppression_requirement, 1901, 0.58).
narrative_ontology:measurement(jati_su_t1911, jati_practice_norm__colonial_census_reading, suppression_requirement, 1911, 0.6).
narrative_ontology:measurement(jati_su_t1921, jati_practice_norm__colonial_census_reading, suppression_requirement, 1921, 0.62).
narrative_ontology:measurement(jati_su_t1931, jati_practice_norm__colonial_census_reading, suppression_requirement, 1931, 0.64).
narrative_ontology:measurement(jati_su_t1941, jati_practice_norm__colonial_census_reading, suppression_requirement, 1941, 0.45).
narrative_ontology:measurement(jati_su_t1951, jati_practice_norm__colonial_census_reading, suppression_requirement, 1951, 0.38).
narrative_ontology:measurement(jati_su_t1971, jati_practice_norm__colonial_census_reading, suppression_requirement, 1971, 0.46).
narrative_ontology:measurement(jati_su_t1991, jati_practice_norm__colonial_census_reading, suppression_requirement, 1991, 0.55).
narrative_ontology:measurement(jati_su_t2011, jati_practice_norm__colonial_census_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement(jati_su_t2021, jati_practice_norm__colonial_census_reading, suppression_requirement, 2021, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, information_standard).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'the jati/caste system' into three epsilon-invariant constraints sharing the kernel jati_practice_norm. Upstream: the orthodox_textual_reading — colonial enumerators drew rankings from Brahmin textual consultants, so the scriptural claim lent the census scheme its plausibility and is cited inside this reading's own archive. Downstream: this reading froze the fluid boundary-making that the localized_practice_reading takes as the norm itself, channeling subsequent local renegotiation through petitions against the printed list. Edges: this file links both siblings; the siblings link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

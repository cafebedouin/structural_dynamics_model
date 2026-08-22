% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Classical Legalist Offensive Jihad Framework (Expansionist Reading)
 *   domain: religious law/political theology/comparative jurisprudence
 *
 * SUMMARY:
 *   The classical legalist framework, crystallized in the formative fiqh of
 *   the eighth through tenth centuries, holds jihad as a collective
 *   obligation to establish Islamic governance where absent, executed through
 *   offensive campaigns that are lawful — indeed obligatory in aggregate —
 *   when launched after formal invitation, under the authority of a qualified
 *   imam, and conducted within proportionality and non-combatant limits. It
 *   partitions the world into a domain of Islam and a domain of war, assigns
 *   non-Muslims outside treaty a liminal status resolvable into combatant,
 *   treaty-protected, or covenant-protected classes, and builds a complete
 *   administrative apparatus for spoils, captives, and covenant taxation. The
 *   claim/metric gap is deliberate and structural: the framework is CLAIMED
 *   here as tangled_rope because it demonstrably possesses both a genuine
 *   coordination function (rule-bound, centrally authorized, limit-respecting
 *   warfare replacing private predation) and asymmetric extraction
 *   (sovereignty, wealth, and persons moved from non-Muslims to the Muslim
 *   polity through the same structure), while the metrics are authored
 *   independently as descriptive measurements of its actual operation. This
 *   story is one reading of the jihad_quranic_corpus kernel; committer
 *   structure is carried in the omega variables and kernel_context, not
 *   averaged across readings.
 *
 * KEY AGENTS:
 *   - caliph_imamate_authority: Agenda-setter and principal recipient (institutional / identity_locked) — alone authorizes offensive campaigns; treasury collects the state fifth, covenant taxes, and conquered-land revenues
 *   - classical_jurisprudential_establishment: Co-administrator and secondary beneficiary (institutional / identity_locked) — maintains the conditions corpus, staffs courts, reproduces doctrine each generation
 *   - muslim_ummah_polity: Collective beneficiary (organized / constrained) — receives security, spoils shares, tax bases, and religious merit; owes participation and funding
 *   - volunteer_mujahidin_settlers: Mixed beneficiary-bearer (moderate / constrained) — spoils, stipends, land, and merit against personal mortality risk
 *   - frontier_non_muslim_states: Primary external bearer of costs (institutional / constrained) — sovereignty itself contested by the expansion provisions
 *   - conquered_dhimmi_communities: Settled cost-bearers (powerless / trapped) — covenant taxation and legal disabilities, bound to land and covenant
 *   - war_captives_enslaved: Extreme cost-bearers (powerless / trapped) — disposition decided wholly by others, no negotiating seat
 *   - treaty_tributary_communities: Protected-but-liable middle position (moderate / constrained) — tribute buys exemption that lapses at the declarer's discretion
 *   - modern_reformist_jurists: Excluded objectors (moderate / mobile) — reject the offensive provisions from outside the transmitting institutions
 *   - comparative_law_historians: Analytical observers (analytical / analytical) — reconstruct formation and practice-gap from documentary record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.66).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Classical Legalist Offensive Jihad Framework (Expansionist Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious law/political theology/comparative jurisprudence").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '1871967e-2fbf-4bf6-8535-2cf3e13b4cfd').
narrative_ontology:cs_kernel_codification('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', fixed_text).
narrative_ontology:cs_authority_grounding('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', lineage).
narrative_ontology:cs_interpretation_layer_present('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd').
narrative_ontology:cs_reading_relation('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', foundational, offensive_campaigns_lawful_under_conditions).
narrative_ontology:cs_axiom_status(offensive_campaigns_lawful_under_conditions, holdable).
narrative_ontology:cs_axiom_grounding('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', offensive_campaigns_lawful_under_conditions, theological).
narrative_ontology:cs_axiom('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', foundational, imam_declaration_authority_required).
narrative_ontology:cs_axiom_status(imam_declaration_authority_required, holdable).
narrative_ontology:cs_axiom_grounding('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', imam_declaration_authority_required, conventional).
narrative_ontology:cs_axiom('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', secondary, dhimma_covenant_protects_conquered_peoples).
narrative_ontology:cs_axiom_status(dhimma_covenant_protects_conquered_peoples, holdable).
narrative_ontology:cs_axiom_grounding('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', dhimma_covenant_protects_conquered_peoples, conventional).
narrative_ontology:cs_reference_frame('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', conditional_offensive_expansion_norm).
narrative_ontology:cs_drift_state('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', post_caliphate_contemporary_era, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('1871967e-2fbf-4bf6-8535-2cf3e13b4cfd', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliph_imamate_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurisprudential_establishment).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_ummah_polity).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, volunteer_mujahidin_settlers).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, treaty_tributary_communities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, frontier_non_muslim_states).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_dhimmi_communities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, war_captives_enslaved).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, volunteer_mujahidin_settlers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, treaty_tributary_communities).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, dar_al_islam_dar_al_harb_bipartition).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, imamate_war_declaration_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, fard_kifaya_collective_campaign_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the office that alone may authorize offensive campaigns under the framework: receives frontier reports, issues the declaration, appoints commanders, and administers the treasury collecting the state fifth of spoils plus the land-tax and poll-tax revenues of conquered districts. Leading the community's expansion is a core claim of the office's legitimacy; in periods with no recognized holder of the office, the offensive provisions stand formally suspended. Renouncing the framework would mean renouncing the office's central function.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliph_imamate_authority, agenda_setter,
    institutional, generational, identity_locked, continental).

% Madhhab jurists elaborate the operating conditions — invitation wording, declarer qualification, division of spoils, treatment of captives and treaty peoples — staff the courts that apply them, and teach the manuals that reproduce the corpus each generation. Scholarly standing and career paths run through maintenance of the consolidated doctrine; moving toward a defensive-only reading would unsettle centuries of settled teaching they steward.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurisprudential_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurisprudential_establishment, beneficiary).

% The community as a whole receives the arrangement's outputs: security through forward operations, shares of spoils and new tax bases, and religious merit attached to supporting campaigns. Members owe participation when summoned and contributions to fund expeditions; ordinary life far from the frontier touches the arrangement mainly through sermon, festival, and school, but opting out of the communal enterprise is not a recognized choice.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_ummah_polity, beneficiary,
    organized, generational, constrained, continental).

% Fighters who join campaigns receive stipends, four-fifths shares of movable spoils, frontier land grants, and counted religious reward; they carry the mortality risk of the campaigning season itself. Many settle conquered districts as garrison households. Participation is voluntary in form but tied to livelihood and status pathways for which few parallel routes exist.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, volunteer_mujahidin_settlers, beneficiary,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, volunteer_mujahidin_settlers, payer).

% Sovereign powers bordering the expanding polity — imperial neighbors and regional kingdoms — receive formal invitation letters demanding submission or tribute before hostilities open. They answer with armies, fortification, truce diplomacy, and tribute purchases of peace intervals. Their continuing sovereignty is precisely what the expansion provisions contest; no term of the framework recognizes their permanent equal standing.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, frontier_non_muslim_states, payer,
    institutional, generational, constrained, continental).

% Peoples whose territories came under the framework's operation after conquest retain worship and communal law under covenant terms, pay the poll-tax and land-tax that substitute for military service, and live under disabilities in testimony, building, and public office. Community leaders negotiate surrender terms; the communities themselves are bound to ancestral land and covenant, and departure means abandoning property and shrines.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_dhimmi_communities, payer,
    powerless, biographical, trapped, regional).

% Persons captured in campaigns fall to the commander's discretion under juristic rules distributing them among ransom, exchange, enslavement, and release. They hold no seat in any negotiation that determines their disposition and no exit except escape or manumission granted by an owner.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, war_captives_enslaved, payer,
    powerless, immediate, trapped, regional).

% Communities that submitted by treaty without conquest pay annual tribute in exchange for exemption from campaign and retained local autonomy. They occupy a middle position: treaties lapse or are renegotiated at the declaring authority's discretion, and renewed hostilities return them to the object of campaign. Tribute buys a protection they cannot otherwise guarantee.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, treaty_tributary_communities, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, treaty_tributary_communities, payer).

% Reform-minded scholars argue the offensive provisions reflected seventh-century circumstances, read the corpus as licensing only defensive force, and reclassify the expansion verses as historical. They publish outside the classical curricula and hold no seat in the madhhab institutions that transmit the framework; their objection registers in public discourse rather than in the arrangement's operative channels.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, modern_reformist_jurists, excluded,
    moderate, generational, mobile, global).

% Academic historians and comparative lawyers reconstruct the framework's formation from chronicles, papyri, and treaty documents, tracking where its rules matched practice and where practice outran doctrine. They take no side in the normative dispute and bear none of its costs; their analyses circulate across all traditions.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, comparative_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliph_imamate_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides rule-bound structure for armed expansion: a single authorized declarer preventing private and unauthorized warfare, a mandatory pre-hostility invitation, proportionality and non-combatant limits, and defined post-war statuses (safe-conduct, treaty, covenant protection) replacing indiscriminate violence. Organizing large campaigns under shared legal discipline and giving defeated peoples predictable treatment solves a real collective-action problem for both sides of the frontier.
% TRANSFER_FUNCTION: Moves sovereignty, land, movable wealth (spoils with the state's reserved fifth), labor (captives), and recurring tax streams (poll-tax and land-tax) from non-Muslim polities and populations into the Muslim polity; moves mortality risk onto the fighting volunteers; moves interpretive authority, judicial office, and administrative revenue to the caliphal and juristic classes.
% ABSENT_VOICES: The targeted polities and conquered populations had no seat in the framework's formation — the fiqh corpus was authored entirely within the conquering tradition, and dhimmi communities appear in it only as objects of regulation. Modern reformist jurists and secular international-law voices object to the offensive provisions but sit outside the classical frame's authoritative conversation; their exclusion is structural, not incidental.
% DISAPPEARANCE_RATIONALE: Overnight removal would dissolve the bipartition of the world into war-abode and peace-abode, void every frontier treaty and covenant, collapse the covenant-tax fiscal system, strip the caliphal office of its declaring function, and idle the spoils-distribution and captive-disposition machinery — the entire legal architecture of Muslim-non-Muslim relations would need rebuilding from different premises.
% FOUNDING_PROBLEM: An infant polity ringed by hostile superpowers needed to channel loosely governed tribal martial capacity into disciplined collective action, and to define a workable legal order for governing relations with the non-Muslim populations its operations encountered.
% FOUNDING_PROBLEM_CORROBORATION: The initial-threat half is corroborated from outside the benefiting parties by Byzantine and Sasanian diplomatic correspondence and chronicles, and by academic historiography of early Islam. The persistence half — that the framework continued as routine expansion-and-revenue administration long after any existential threat passed — is likewise documented by historians external to the tradition, while traditionalist jurists inside the benefiting parties dispute it and affirm the obligation's continuity. No external source attests the standing-expansion phase as necessity-driven.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.68 reflects an arrangement whose operation moves sovereignty, wealth, and persons from non-Muslim polities and populations to the Muslim polity while binding its own members to participation and its conquered subjects to covenant taxation; the load is heavy but bounded by real conditions — invitation before hostilities, centralized declaration, proportionality and non-combatant rules, defined post-war statuses — which keeps it well short of pure-predation levels. Suppression 0.66 is predominantly structural: covenant disabilities, tax compulsion, captive disposition at another's discretion, and the closure of the defensive-only reading within classical curricula; internalized elements (pious-duty framing that survives barrier removal) exist but are secondary, and the omega on suppression mechanism records the split. Theater 0.24: the machinery performed real coordinative and administrative work; the pre-battle invitation carries a formalist component that grew as conquest became routinized. Accessibility collapse 0.52: alternatives existed (treaty, tribute, conversion, truce) but the world-bipartition narrowed the space of standing arrangements available to non-Muslims. Resistance 0.58: sustained frontier warfare, siege economies, and later reformist rejection. The temporal series runs on one shared grid — all three tracked metrics authored at every decade point — and rises together as the framework matured from threat-response into standing expansion administration. Frontier activity oscillated seasonally around this rising baseline; the series models the baseline trend, not the seasonal cycle, and the cycle itself (campaign season, truce interval, treaty lapse) functioned as a rhythm of the arrangement rather than as intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seats (caliphate, jurists) and the receiving seats (ummah, fighters) compute a rule-governed public order carrying religious reward; the paying seats compute dispossession, taxation, and captivity. The same invitation letter reads as a mercy-offering from the sender's desk and as an ultimatum preceding invasion from the recipient's wall. Fighters hold both experiences at once — gain and grave risk in the same campaign. The engine computes these divergent per-seat classifications from the declared structural positions; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (caliphate, jurists, ummah, fighters, treaty communities) drive those seats toward the subsidized end of the directionality scale; victim declarations (frontier states, dhimmi communities, captives) drive those seats toward the full-target end, amplified for trapped exits (dhimmis and captives cannot leave) and moderated for the mobile, externally situated reformists. The structural derivation chain handles every seat from the declared beneficiary/victim data and exit options, so no explicit directionality overrides are authored. The one genuine ambiguity — treaty communities who pay tribute yet receive protection, and fighters who receive spoils yet bear mortality — sits inside the moderate-power atom, where a single per-atom override could not separate the two seats; both are left to structural derivation and flagged in the omegas instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — superpower hostility plus undirected tribal martial capacity — was substantially transformed once the polity consolidated; the framework persisted for centuries afterward as routine expansion-and-revenue administration. The genealogy interview records founding_problem_status as contested rather than dead: traditionalist jurists maintain the obligation's continuity (suspended pending a qualified declarer), while historians external to the tradition document the shift from threat-response to standing fiscal-military operation. Because status is contested, the mismatch consumer finds no dead-plus-world_rearranges flag here; the honest signals are the named receipt seat and the prohibitive cost of dismantling the arrangement for those who administer it, which keep its capture profile visible without asserting an obsolescence the parties themselves dispute. The low theater ratio further argues against a decayed-performance reading: what persists is operational doctrine awaiting its enabling condition, not empty ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Which reading of the jihad_quranic_corpus kernel does this constraint instantiate, and what would each sibling reading change structurally?',
    'Compare the three sibling stories'' victim sets, declaration-authority conditions, and epsilon values; this file instantiates the expansionist_legalist_reading.',
    'The defensive_spiritual_reading yields near-zero offensive extraction and no liminal-status victim class; the revolutionary_vanguard_reading deletes the imam monopoly, individualizes the obligation, and retargets costs at apostate rulers and occupying powers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame record: this story is one reading of a three-reading kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    default_status_of_non_muslim_territory,
    'Is the default juridical status of non-Muslim territory outside treaty a licit campaign object (the war-abode) or an abode presumptively at peace absent active hostilities?',
    'Doctrinal analysis of the bipartition-versus-tripartition debates across the madhhabs, combined with treaty-frequency data in campaign records.',
    'Resolution toward presumptive peace collapses this reading''s distinctive extraction surface and pulls it toward the defensive sibling; resolution toward licit object fixes the liminal-status victim class this story declares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_status_of_non_muslim_territory, conceptual, 'Where the kernel disagreement is located: the default status of the non-Muslim other''s territory.').

omega_variable(
    imam_condition_operativity,
    'Does the imam-authority condition suspend the offensive provisions in the absence of a recognized caliph, or does the declaring function pass to any ruler waging expansion?',
    'Cross-madhhab analysis of valid-declaration criteria plus historical study of sultans assuming the declaring function under caliphal delegation.',
    'Strict suspension keeps the framework dormant pending restoration; functional transfer revives the full enforcement structure under any qualifying state, transforming the constraint''s present-day operability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imam_condition_operativity, empirical, 'Whether the framework''s central enabling condition is currently inoperative or transferable.').

omega_variable(
    invitation_sincerity,
    'Did the pre-campaign invitation function as a genuine choice-offering or as a legitimating formality preceding predetermined invasion?',
    'Campaign chronicles: timing of invitations relative to mobilization, surrender-term patterns, and documented cases where timely submission averted attack.',
    'Predominant formalism raises the performative component and pushes the arrangement toward cover-story operation; documented choice-effect supports the weight assigned to its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invitation_sincerity, empirical, 'Sincerity of the invitation condition, the framework''s most theatrical-seeming element.').

omega_variable(
    dhimma_protection_or_subordination,
    'Are the covenant terms offered to conquered communities a protective settlement they could not otherwise obtain, or a subordination regime imposed on the defeated?',
    'Comparative analysis against contemporaneous conquest settlements elsewhere, dhimmi communal records, and rates of covenant renewal versus revolt.',
    'A protection-reading gives the dhimmi seats a partial-beneficiary component and lowers measured asymmetry; a subordination-reading confirms the full-target directionality declared here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dhimma_protection_or_subordination, conceptual, 'Framing ambiguity in the covenant''s benefit structure for the conquered.').

omega_variable(
    proportionality_practice_gap,
    'Did the proportionality and non-combatant rules constrain campaign practice, or did they remain treatise ideals honored in the manuals and breached on campaign?',
    'Chronicle-by-chronicle comparison of prescribed conduct against reported conduct across major campaigns, weighted for source bias.',
    'A wide practice gap widens effective extraction beyond the doctrinal baseline and weakens the coordination half of the hybrid; close tracking strengthens it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_practice_gap, empirical, 'Gap between the framework''s limiting rules as taught and as practiced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_expansionist_legalist_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_tr_t0, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_tr_t10, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_tr_t10, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_tr_t20, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_tr_t20, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_tr_t30, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_tr_t30, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_tr_t40, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_tr_t50, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 50, 0.23).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_tr_t50, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_tr_t60, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(jihad_expansionist_legalist_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_be_t0, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_be_t10, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_be_t10, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_be_t20, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_be_t20, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_be_t30, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_be_t30, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_be_t40, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_be_t50, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_be_t50, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_be_t60, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(jihad_expansionist_legalist_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_su_t0, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_su_t10, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_su_t10, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_su_t20, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_su_t20, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_su_t30, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_su_t30, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_su_t40, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_su_t50, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_su_t50, observed).
narrative_ontology:measurement(jihad_expansionist_legalist_su_t60, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(jihad_expansionist_legalist_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, dhimma_jizya_administration).

% DUAL FORMULATION NOTE:
% The colloquial label 'jihad' conflates three structurally distinct legal regimes sharing one scriptural kernel (jihad_quranic_corpus). This story instantiates the expansionist_legalist_reading: offensive campaigns lawful under invitation, imam-authority, and proportionality conditions, non-Muslims in liminal status, state monopoly on declaration. The defensive_spiritual_reading restricts armed force to defense and relocates primary struggle inward (near-zero offensive extraction, no liminal-status victim class). The revolutionary_vanguard_reading deletes the state monopoly, individualizes the obligation, and retargets costs at apostate rulers and occupiers. Each sibling is a separate constraint story with its own epsilon, victim set, and enforcement structure; this file links them as kernel kin. Downstream, this reading legitimates the dhimma-jizya administrative complex, linked separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

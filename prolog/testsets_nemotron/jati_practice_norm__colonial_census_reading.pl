% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Jati Categories Fixed by Colonial Census Administration
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   The British Indian census (from 1871 onward) imposed fixed, hierarchical
 *   jati categories onto a previously fluid landscape of occupational,
 *   kinship, and devotional groupings. What had been locally negotiated,
 *   context-dependent identities — with permeability, fission, fusion, and
 *   regional variation — were frozen into a single imperial taxonomy for
 *   revenue extraction, labor mobilization, and political management. The
 *   census did not merely record; it created the categories it purported to
 *   measure. Postcolonial India retained and expanded this apparatus
 *   (Scheduled Castes/Tribes lists, OBC commissions, caste censuses),
 *   converting colonial administrative legibility into democratic
 *   representation and affirmative action logics. The constraint extracts
 *   from mobile communities (criminal tribes, nomadic groups, syncretic
 *   sects) and lower jatis (fixed into untouchable/backward slots), while
 *   benefiting colonial administrators (legible tax base), postcolonial
 *   bureaucracies (stable beneficiary populations for quotas), and dominant
 *   jati elites (state-recognized status monopoly).
 *
 * KEY AGENTS:
 *   - colonial_administration: Primary agenda_setter (institutional/arbitrage) — designed and enforced the census taxonomy
 *   - postcolonial_state_bureaucracy: Secondary agenda_setter (institutional/arbitrage) — inherited, refined, and weaponized the categories for quotas and representation
 *   - dominant_jati_elites: Beneficiary (organized/constrained) — captured state recognition to lock in status and exclude rivals
 *   - mobile_communities: Victim (powerless/trapped) — nomadic, forest-dwelling, and criminal-tribe groups forcibly categorized and settled
 *   - lower_jati_populations: Victim (organized/identity_locked) — Dalit/backward groups fixed into administrative slots that enable quotas but freeze identity
 *   - syncretic_communities: Victim (moderate/constrained) — devotional/occupational groups straddling categories, erased by binary classification
 *   - dominant_caste_rival_groups: Victim (organized/constrained) — groups contesting dominant jati's state-recognized monopoly
 *   - anthropological_observer: Observer (analytical/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.62).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.71).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Categories Fixed by Colonial Census Administration").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, 'dc1a7384-fa27-4165-bf76-a7cfcaa359f3').
narrative_ontology:cs_kernel_codification('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', formalized).
narrative_ontology:cs_authority_grounding('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', extraction).
narrative_ontology:cs_interpretation_layer_present('dc1a7384-fa27-4165-bf76-a7cfcaa359f3').
narrative_ontology:cs_reading_relation('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', foundational, administrative_enumeration_creates_social_reality).
narrative_ontology:cs_axiom_status(administrative_enumeration_creates_social_reality, holdable).
narrative_ontology:cs_axiom_grounding('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', administrative_enumeration_creates_social_reality, empirically_contingent).
narrative_ontology:cs_axiom('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', secondary, state_legibility_requires_fixed_categories).
narrative_ontology:cs_axiom_status(state_legibility_requires_fixed_categories, holdable).
narrative_ontology:cs_axiom_grounding('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', state_legibility_requires_fixed_categories, instrumental).
narrative_ontology:cs_reference_frame('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', pre_census_fluid_jati_landscape).
narrative_ontology:cs_drift_state('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', postcolonial_quota_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc1a7384-fa27-4165-bf76-a7cfcaa359f3', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, postcolonial_state_bureaucracy).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, dominant_jati_elites).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, mobile_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, dominant_caste_rival_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, lower_jati_populations).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, syncretic_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, lower_jati_populations).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, dominant_jati_elites).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, administrative_legibility_requires_fixed_categories).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, census_enumeration_creates_social_reality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designed the census taxonomy (1871 onward) to create a legible, taxable, governable population. The categorization enabled revenue extraction, labor mobilization for plantations/railways, and political management (divide-and-rule). The administration could adjust categories between censuses but the structural commitment to fixed enumeration was irreversible. Exit from this role meant imperial withdrawal — not available to the administration itself.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, colonial_administration, beneficiary).

% Inherited the colonial census apparatus and repurposed it for democratic representation: Scheduled Castes/Tribes lists, OBC commissions, caste-based quotas in education and employment. The bureaucracy benefits from stable beneficiary populations for welfare targeting and electoral management. It administers the categories but is also constrained by them — quota politics create demands for new categorizations (e.g., caste census demands).
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, postcolonial_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, postcolonial_state_bureaucracy, beneficiary).

% Captured state recognition of their jati status to monopolize ritual prestige, land control, and political representation. They benefit from the freeze on category fluidity (rivals cannot easily claim equal status). They pay enforcement costs: maintaining endogamy, policing boundaries, litigating against rival groups' classification claims. Exit means losing state-recognized monopoly — structurally difficult.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, dominant_jati_elites, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, dominant_jati_elites, payer).

% Nomadic, forest-dwelling, and pastoral groups (e.g., Banjara, Pardhi, various 'criminal tribes') were forcibly categorized as criminal or backward, settled under police surveillance, and stripped of mobility. The census category became a legal disability (Criminal Tribes Act 1871, Habitual Offenders Act 1952). No effective exit: the state tracks them by the very categories it imposed. Their traditional livelihoods are criminalized by the categorization.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, mobile_communities, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, mobile_communities, excluded).

% Dalit and 'backward' jatis fixed into administrative slots that enable quotas (reservations in education, jobs, legislatures) but freeze identity into state categories. The quota system delivers material resources but fuses caste identity to administrative certification — exit from the category means losing the benefit. Identity is locked: the administrative category IS the social identity for political mobilization. Resistance operates within the frame (demanding inclusion, reclassification, or caste census).
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, lower_jati_populations, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, lower_jati_populations, beneficiary).

% Devotional sects, occupational guilds, and regional communities that straddle jati/varna boundaries (e.g., Lingayats, Kabirpanthis, various Bhakti groups). The census forced them into single categories, erasing their internal diversity and cross-cutting affiliations. They can petition for reclassification but face high evidentiary barriers and political contestation from dominant jatis. Exit is constrained: they must choose one administrative slot or be rendered illegible.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, syncretic_communities, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, syncretic_communities, excluded).

% Groups contesting the dominant jati's state-recognized status monopoly (e.g., Kurmi vs. Yadav, Vanniyar vs. Thevar). They are payers because the dominant jati's classification captures quotas and political representation they seek. They are excluded from the beneficiary seat. They contest within the administrative frame (demanding separate categorization, OBC sub-quotas). Exit means accepting subordinate status or escalating to violence — both constrained.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, dominant_caste_rival_groups, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, dominant_caste_rival_groups, excluded).

% Sees the full structure across all three readings: the colonial census as administrative extraction, the varna framework as ritual legitimation, and local practice as ongoing coordination. Does not collect from or pay into the constraint. Provides the analytical seat from which the kernel decomposition is visible.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, anthropological_observer, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jati_practice_norm__colonial_census_reading, anthropological_observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, postcolonial_state_bureaucracy).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, stable population taxonomy for state governance: revenue assessment, labor allocation, political representation, and welfare targeting. Replaces fluid local negotiation with a single authoritative classification that all state apparatuses can reference.
% TRANSFER_FUNCTION: Moves administrative ease, tax revenue, and political compliance from categorized populations (especially mobile and lower jatis) to the state bureaucracy and its captured elites (dominant jatis). Quotas later reverse a portion of the flow (resources to lower jatis) but only within the fixed categories the constraint creates.
% ABSENT_VOICES: Pre-colonial fluid identities (nomadic groups before settlement, syncretic sects before categorization, communities that fissioned/fused across census lines). They are absent because the census itself erased the social conditions for their existence. Also absent: the 'non-caste' option — the census made caste universal and mandatory.
% DISAPPEARANCE_RATIONALE: If the census-fixed jati categories vanished overnight, the quota system (reservations, OBC lists, SC/ST schedules) would lose its administrative basis — democratic representation and affirmative action would need new logics. Land tenure, political reservations, and welfare targeting would reorganize. Dominant jati status monopolies would face open contestation. Mobile communities might reclaim mobility but lose state recognition entirely. The postcolonial state's governance infrastructure is built on this taxonomy.
% FOUNDING_PROBLEM: Colonial governance required a legible, enumerated, and controllable population for revenue extraction (land tax), labor mobilization (plantations, railways, military), and political management (divide-and-rule through community categorization). The pre-census landscape of fluid, context-dependent groupings was illegible to imperial administration.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (imperial governance legibility) is dead — corroborated by the dissolution of the British Empire and the end of colonial revenue/military imperatives. The postcolonial state's repurposing of the apparatus for democratic representation is attested by Constituent Assembly debates (Ambedkar, Rajendra Prasad) and the Mandal Commission report — sources outside the colonial administration. Dominant jati elites contest the 'dead' status, claiming the categories reflect eternal varna reality (orthodox_textual_reading).
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) is moderate-high: the census apparatus extracts labor, revenue, and political compliance from categorized populations while delivering administrative efficiency to the state. Suppression (0.71) is high: categories are enforced through land settlement acts, criminal tribes legislation, police surveillance, and later through quota certification regimes. Theater ratio (0.38) is moderate: the census presents as scientific enumeration (coordination) but functions as political extraction. Accessibility collapse (0.58) is moderate: alternatives (fluid local norms, scriptural varna, non-caste affiliations) persist but are marginalized by state power. Resistance (0.54) is moderate: anti-caste movements, census boycotts, and category petitions exist but operate within the administrative frame. The constraint is Tangled Rope: genuine coordination (state needs legible populations for governance) fused with asymmetric extraction (state and dominant jatis benefit, mobile/lower jatis pay).
 *
 * PERSPECTIVAL GAP:
 *   From the colonial_administration seat, the constraint is genuine coordination (legible population for governance) — low extraction, high benefit. From mobile_communities and lower_jati_populations seats, it is enforced categorization that destroys autonomy and extracts labor/status — high extraction, high suppression. From dominant_jati_elites, it is a captured coordination mechanism that secures monopoly status — net beneficiary but with enforcement costs. The engine computes this divergence from structural data; the claim (tangled_rope) reflects the structural reality that both coordination and extraction are real and irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   colonial_administration and postcolonial_state_bureaucracy are structural beneficiaries (d ~ 0.15): they collect administrative efficiency, tax revenue, and political control. dominant_jati_elites are secondary beneficiaries (d ~ 0.25): they capture status monopolies but bear some enforcement costs. mobile_communities are full targets (d ~ 0.95): trapped by settlement acts, police surveillance, and loss of nomadic livelihood. lower_jati_populations are identity-locked targets (d ~ 0.85): quotas provide material benefits but fuse identity to administrative category, making exit unthinkable. syncretic_communities are constrained targets (d ~ 0.7): they can petition for reclassification but face high transaction costs. dominant_caste_rival_groups are constrained payers (d ~ 0.65): they contest the beneficiary's monopoly within the same administrative frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial governance legibility) is dead — the British Empire is gone. But the arrangement persists because postcolonial_bureaucracy captured the apparatus for democratic representation (quotas, reservations) and dominant_jati_elites capture it for status monopoly. This is mandatrophy: the coordination function mutated from imperial governance to democratic representation, but the extraction structure (fixed categories, state-enforced identity) remains. The constraint is not a Piton (not merely theatrical) — the quota system delivers real resources to real beneficiaries. It is not a Scaffold (no sunset clause) — the postcolonial state treats fixed categories as permanent infrastructure. Tangled Rope is correct: coordination (democratic representation requires legible groups) and extraction (dominant jatis capture quotas, state extracts compliance) are fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_jati_boundaries,
    'Are jati boundaries genuine natural-kind social structures that the census merely recorded, or were they substantially constructed by the enumeration process itself?',
    'Comparative analysis of pre-census inscriptional/epigraphic records vs. post-census administrative categories; ethnographic evidence of category fluidity in non-enumerated regions.',
    'If boundaries were substantially constructed by enumeration, the constraint is a false summit candidate (colonial census as Mountain claim masking Tangled Rope extraction); if pre-existing natural kinds, the census stabilized a Mountain-like structure with extractive administration layered atop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_jati_boundaries, empirical, 'Whether jati categories are natural kinds or administrative constructions').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (colonial_census_reading) of the contested kernel jati_practice_norm. Sibling readings: orthodox_textual_reading, localized_practice_reading. The disagreement is located in whether jati boundaries are fixed by external administration (this reading), fixed by scripture (orthodox_textual), or fluid local norms (localized_practice).',
    'Comparative analysis of how each reading''s beneficiary/victim structure and enforcement mechanism map onto historical and contemporary power distributions.',
    'If sibling readings produce different beneficiary/victim structures and extraction profiles, the kernel label ''jati_practice_norm'' covers multiple structurally distinct constraints — the ε-invariance principle requires separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment kernel decomposition: this reading vs. siblings on the nature of jati boundaries').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal enforcement, police power, land tenure systems) or internalized (caste identity fused with census category, ritual pollution beliefs reinforcing administrative categories)?',
    'Post-independence trajectory analysis: if suppression persists after colonial legal apparatus is formally dismantled, internalized component is significant. Compare regions with different postcolonial state capacities.',
    'If internalized, effective suppression exceeds structural measure — targets carry the constraint with them after formal exit. This raises the constraint''s effective extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in caste administration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1871, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t1871, jati_practice_norm__colonial_census_reading, theater_ratio, 1871, 0.12).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t1891, jati_practice_norm__colonial_census_reading, theater_ratio, 1891, 0.18).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t1911, jati_practice_norm__colonial_census_reading, theater_ratio, 1911, 0.25).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t1931, jati_practice_norm__colonial_census_reading, theater_ratio, 1931, 0.32).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t1951, jati_practice_norm__colonial_census_reading, theater_ratio, 1951, 0.35).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t1971, jati_practice_norm__colonial_census_reading, theater_ratio, 1971, 0.38).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t1991, jati_practice_norm__colonial_census_reading, theater_ratio, 1991, 0.37).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t2011, jati_practice_norm__colonial_census_reading, theater_ratio, 2011, 0.38).

% Extraction over time
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t1871, jati_practice_norm__colonial_census_reading, base_extractiveness, 1871, 0.35).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t1891, jati_practice_norm__colonial_census_reading, base_extractiveness, 1891, 0.48).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t1911, jati_practice_norm__colonial_census_reading, base_extractiveness, 1911, 0.56).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t1931, jati_practice_norm__colonial_census_reading, base_extractiveness, 1931, 0.61).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t1951, jati_practice_norm__colonial_census_reading, base_extractiveness, 1951, 0.58).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t1971, jati_practice_norm__colonial_census_reading, base_extractiveness, 1971, 0.55).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t1991, jati_practice_norm__colonial_census_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t2011, jati_practice_norm__colonial_census_reading, base_extractiveness, 2011, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t1871, jati_practice_norm__colonial_census_reading, suppression_requirement, 1871, 0.45).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t1891, jati_practice_norm__colonial_census_reading, suppression_requirement, 1891, 0.55).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t1911, jati_practice_norm__colonial_census_reading, suppression_requirement, 1911, 0.62).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t1931, jati_practice_norm__colonial_census_reading, suppression_requirement, 1931, 0.68).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t1951, jati_practice_norm__colonial_census_reading, suppression_requirement, 1951, 0.72).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t1971, jati_practice_norm__colonial_census_reading, suppression_requirement, 1971, 0.7).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t1991, jati_practice_norm__colonial_census_reading, suppression_requirement, 1991, 0.71).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t2011, jati_practice_norm__colonial_census_reading, suppression_requirement, 2011, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.08).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, colonial_land_revenue_settlement).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, criminal_tribes_act_enforcement).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, postcolonial_reservation_quotas).

% DUAL FORMULATION NOTE:
% Part of the jati_practice_norm constraint family (kernel decomposition). This reading (colonial_census_reading) treats jati fixation as an administrative act with extractive consequences. The orthodox_textual_reading treats fixation as scriptural/ritual (lower extraction, different victim structure). The localized_practice_reading treats boundaries as coordination norms (rope-like, minimal extraction). The three stories share the kernel label but have distinct ε, beneficiary/victim structures, and enforcement mechanisms. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, organized, 0.25).
constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

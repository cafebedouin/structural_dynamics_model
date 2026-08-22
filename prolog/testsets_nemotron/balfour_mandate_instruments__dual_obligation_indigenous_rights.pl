% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate Dual Obligation - Indigenous Rights Primacy
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint captures the reading of the British Mandate for Palestine
 *   (1920-1947) that treats the mandate's dual obligation — facilitating a
 *   'national home for the Jewish people' while protecting 'the civil and
 *   religious rights of existing non-Jewish communities' — as imposing an
 *   equal or superior duty to protect Arab civil/political rights and land
 *   tenure. Under this reading, the 'national home' clause is subordinated to
 *   the self-determination norm in Article 22 of the League Covenant and to
 *   minority-protection principles from the post-WWI treaty system. Land
 *   transfer restrictions (e.g., 1930 Passfield White Paper, 1940 Land
 *   Transfer Regulations) and immigration quotas (1939 White Paper) are the
 *   operational expressions of this reading. The constraint is a
 *   tangled_rope: it coordinates Arab land tenure and political
 *   representation (genuine coordination function) while actively suppressing
 *   Zionist land acquisition and demographic growth (asymmetric extraction),
 *   requiring continuous British enforcement. Beneficiaries are Palestinian
 *   Arab elites and communities; victims are Zionist organizations blocked
 *   from land acquisition and demographic parity, and British administrators
 *   constrained in satisfying Zionist demands they were simultaneously
 *   mandated to facilitate.
 *
 * KEY AGENTS:
 *   - palestinian_arab_elites: Primary beneficiary (organized/constrained) — gains land tenure protection and political representation claims
 *   - palestinian_arab_communities: Primary beneficiary (powerless/trapped) — gains land security but remains subject to mandate authority
 *   - zionist_organizations: Primary victim (powerful/constrained) — blocked from land acquisition and demographic parity
 *   - british_administrators: Secondary victim/agenda_setter (institutional/arbitrage) — constrained by dual mandate, extracts administrative rent from interpretive discretion
 *   - league_of_nations_pmc: Observer (institutional/analytical) — supervises mandate compliance but lacks enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate Dual Obligation - Indigenous Rights Primacy").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, '890d4e18-2449-46bd-a41c-1b3c0c15a9c3').
narrative_ontology:cs_kernel_codification('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', formalized).
narrative_ontology:cs_authority_grounding('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', lineage).
narrative_ontology:cs_interpretation_layer_present('890d4e18-2449-46bd-a41c-1b3c0c15a9c3').
narrative_ontology:cs_reading_relation('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', foundational, article_22_self_determination_primacy).
narrative_ontology:cs_axiom_status(article_22_self_determination_primacy, holdable).
narrative_ontology:cs_axiom_grounding('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', article_22_self_determination_primacy, conventional).
narrative_ontology:cs_axiom('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', foundational, minority_protection_binds_national_home).
narrative_ontology:cs_axiom_status(minority_protection_binds_national_home, holdable).
narrative_ontology:cs_axiom_grounding('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', minority_protection_binds_national_home, conventional).
narrative_ontology:cs_reference_frame('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', mandate_article_22_self_determination_framework).
narrative_ontology:cs_drift_state('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', post_1939_white_paper, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('890d4e18-2449-46bd-a41c-1b3c0c15a9c3', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, self_determination_norm).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_principle).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, indigenous_land_tenure_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained land tenure protections and limited political representation (e.g., Supreme Muslim Council, Arab Higher Committee) under the mandate. Extracted political rent from their intermediary position between British authorities and Arab communities. But remained constrained: no sovereign authority, dependent on British toleration, ultimately displaced by 1948. Exit meant rejecting mandate institutions — which some did (1936-39 revolt) — but with no viable alternative sovereignty path.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, payer).

% Benefited from land transfer restrictions that prevented dispossession (1930s-1940s regulations preserved ~85% Arab land ownership in 1947). But bore costs: no political self-determination, subject to British emergency regulations, village structures disrupted by mandate taxation and Zionist land purchases that did occur. Exit was structurally impossible — no neighboring state offered citizenship, no international body enforced self-determination. Identity-locked to the land; displacement in 1948 confirmed the trap.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, payer).

% Blocked from land acquisition (1940 Land Transfer Regulations restricted purchases to 5% of Palestine) and demographic parity (1939 White Paper capped immigration at 75,000 over 5 years). Built parallel institutions (Histadrut, Jewish Agency, Haganah) that functioned as a state-in-waiting. Gained international legitimacy (Balfour Declaration, League mandate) and British tolerance for institution-building. Exit meant accepting demographic minority status or pursuing armed confrontation (Irgun/Lehi). Constrained by dependence on British goodwill for immigration certificates and international recognition.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, beneficiary).

% Held mandatory authority to interpret and enforce the dual obligation. Extracted strategic value: control of Suez-adjacent territory, air routes, oil pipeline termini, and imperial prestige. But bore escalating enforcement costs: 100,000+ troops deployed 1936-39, administrative burden of managing irreconcilable claims, international criticism (PMC reports, US pressure). Exit was always available (and exercised in 1947) — Britain could refer the problem to UN and withdraw. The mandate was a choice, not a trap, making British administrators the only seat with genuine arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, payer).

% Supervised mandate compliance through annual reports and petitions. Had no enforcement capacity beyond moral pressure and delayed supervision. Produced critical reports (e.g., 1930 Passfield White Paper genesis) but could not compel British policy changes. Exit: the League itself dissolved in 1946, transferring supervisory role to UN. Pure analytical seat — neither collected rents nor bore costs from the constraint's operation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_pmc, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Arab land tenure security and collective political representation against demographic displacement, providing a legal-administrative framework that recognizes indigenous majority rights within a mandatory system.
% TRANSFER_FUNCTION: Moves land-title security and political representation claims from British mandatory authority to Palestinian Arab communities, while moving land-acquisition rights and immigration access from Zionist organizations to British administrative control (which restricts both).
% ABSENT_VOICES: Palestinian peasantry (fallahin) — structurally excluded from mandate institutions, represented only through elite intermediaries. Jewish refugees from Europe (1930s-1940s) — excluded by immigration quotas, their voices mediated through Zionist organizations. Neither had direct access to the Permanent Mandates Commission or British policy-making.
% DISAPPEARANCE_RATIONALE: If the dual-obligation reading vanished overnight (e.g., British adopted jewish_national_home_primacy as sole reading), land transfer restrictions would lift, immigration quotas would rise, Arab land tenure would collapse, and the mandate would accelerate toward Jewish state formation without minority protections — the 1948 war and Palestinian displacement would likely occur earlier and more completely. If it vanished in the opposite direction (mandate ended, independence granted), the region would rearrange into a sovereign Arab state with Jewish minority — a different rearrangement.
% FOUNDING_PROBLEM: The post-WWI settlement needed to administer former Ottoman Arab territories toward self-government (Article 22) while honoring Britain's wartime commitment to a 'national home for the Jewish people' (Balfour Declaration 1917). The dual obligation was the legal architecture attempting to reconcile these contradictory promises.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (reconciling Article 22 self-determination with Balfour Declaration) is dead: the mandate terminated without achieving self-government for the Arab majority, and the 'national home' was realized as a sovereign state (Israel) through UN partition, not through the mandate's dual-obligation framework. Corroboration: British Foreign Office records (1947 referral to UN), UNSCOP report (1947) documenting the irreconcilability, and the League of Nations' own failure to resolve the contradiction. No party outside the British-Zionist alliance attested the dual obligation as workable; Arab leadership consistently rejected it as insufficient; Zionist leadership consistently treated it as an obstacle.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72) is high because the mandate system extracts compliance from both populations: Zionist organizations are denied land/immigration access they were promised; Arab communities are denied sovereign self-government despite being the majority. Suppression (0.68) reflects active enforcement: land transfer bans, immigration quotas, dissolution of Arab political institutions (1936-39 revolt suppression), censorship. Theater ratio (0.45) rises over time as British administration increasingly performs 'even-handedness' while the constraint's actual function shifts toward managing an unresolvable conflict. Accessibility collapse (0.55) is moderate — alternatives (binational state, partition, independence) existed but were systematically foreclosed by British policy. Resistance (0.75) is high from both Zionist and Arab nationalist movements. The interval-end metrics reflect 1947 terminus when the mandate collapsed.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian Arab seat (powerless/trapped), the constraint appears as a snare: land protections are real but political sovereignty is denied; the mandate extracts compliance without delivering self-determination. From the Zionist seat (powerful/constrained), it appears as a snare: the 'national home' promise is actively subverted by the mandatory power. From the British administrator seat (institutional/arbitrage), it appears as a tangled_rope: genuine coordination of a dual-obligation mandate that requires active enforcement to prevent either side from overturning the balance — but the enforcement itself becomes the extraction mechanism (administrative rent, strategic control). The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab communities are structural beneficiaries of land tenure protections (d ~ 0.2-0.3) but victims of political suppression (d ~ 0.7) — the net directionality depends on which obligation (land vs. sovereignty) is weighted. The schema's beneficiary/victim arrays capture the land-tenure dimension as primary. Zionist organizations are unambiguous victims (d ~ 0.8-0.9) — blocked from land purchase and immigration. British administrators are the agenda_setters who extract interpretive rent (d ~ 0.15 as beneficiary of mandate authority) but bear enforcement costs (d ~ 0.6 as payer of suppression). The dual role creates the tangled_rope structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate's founding problem (administering former Ottoman territories toward self-government per Article 22) was live in 1920 but became dead for Palestine by the 1930s: the dual obligation proved irreconcilable, and the mandate persisted as a structure for managing conflict rather than advancing self-determination. The constraint did not resolve its mandatrophy — it escalated into a snare-like enforcement regime (1939 White Paper) that satisfied neither population. The British withdrawal in 1947 was not a resolution but an abandonment. The classification as tangled_rope (not snare) rests on the genuine coordination function (land tenure protection) persisting alongside extraction — but the rising theater_ratio and extraction trajectory suggest drift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Balfour mandate kernel, or a separate constraint entirely?',
    'Compare epsilon and beneficiary/victim structure across the three declared readings (dual_obligation_indigenous_rights, jewish_national_home_primacy, mandatory_interpretive_discretion). If epsilon differs by >0.25 or beneficiary sets are disjoint, they are separate constraints per ε-invariance.',
    'If separate constraints, each gets its own classification and network links. If same constraint viewed differently, the framework must model perspectival divergence within one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings constitute one constraint with perspectival variance or three ε-distinct constraints in a family').

omega_variable(
    naturalness_vs_construction,
    'Are the Arab civil/political rights and land tenure protections natural-law obligations of the mandate system, or constructed interpretations serving British imperial management?',
    'Analyze League of Nations Permanent Mandates Commission records for whether Article 22 obligations were treated as jus cogens or as discretionary policy. Cross-reference with contemporary minority treaties (e.g., Polish Minority Treaty).',
    'If natural-law, the constraint trends toward Mountain/Roep with lower extraction. If constructed interpretation, the high ε (0.72) reflects British extraction via interpretive control — the mandate system extracts compliance from both Arab and Zionist parties while maintaining administrative discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_construction, conceptual, 'Whether the dual obligation is a fixed feature of international law or a constructed reading that benefits the mandatory power').

omega_variable(
    beneficiary_capture_risk,
    'Do Palestinian Arab elites genuinely benefit from the dual obligation, or are they coordinated into a management structure that extracts their political agency while preserving land tenure?',
    'Examine 1920s-1930s Palestinian Arab political organization: did the mandate''s representative-institution proposals (Legislative Council, Arab Agency) confer genuine self-government or create a controlled interlocutor class?',
    'If elites are coordinated beneficiaries, the constraint is a genuine tangled_rope. If they are managed into a controlled opposition, the beneficiary declaration masks a snare structure where both Arab and Zionist populations are extracted from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_risk, empirical, 'Whether Arab elite beneficiaries are net winners or managed participants in a dual-extraction system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_dual_obligation_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1923, 0.3).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1929, 0.38).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1936, 0.45).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1939, 0.52).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1947, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1947, 0.58).

% Extraction over time
narrative_ontology:measurement(balfour_dual_obligation_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(balfour_dual_obligation_be_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1923, 0.52).
narrative_ontology:measurement(balfour_dual_obligation_be_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1929, 0.61).
narrative_ontology:measurement(balfour_dual_obligation_be_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1936, 0.72).
narrative_ontology:measurement(balfour_dual_obligation_be_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1939, 0.78).
narrative_ontology:measurement(balfour_dual_obligation_be_t1947, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1947, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(balfour_dual_obligation_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(balfour_dual_obligation_su_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1923, 0.48).
narrative_ontology:measurement(balfour_dual_obligation_su_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(balfour_dual_obligation_su_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1936, 0.68).
narrative_ontology:measurement(balfour_dual_obligation_su_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1939, 0.75).
narrative_ontology:measurement(balfour_dual_obligation_su_t1947, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1947, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.1).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestine_land_transfer_regulations_1940).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestine_immigration_quotas_1939).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form a constraint family decomposing the 'Balfour Mandate' label. Each has distinct epsilon, beneficiaries, victims, and claimed_type. dual_obligation_indigenous_rights: ε=0.72, tangled_rope, beneficiaries=Arab elites/communities. jewish_national_home_primacy: ε≈0.65, tangled_rope, beneficiaries=Zionist organizations. mandatory_interpretive_discretion: ε≈0.55, snare, beneficiaries=British administrators. The family link is structural: the mandatory power's interpretive discretion (third constraint) is the mechanism that instantiates either of the first two readings as operational policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, institutional, 0.25).
constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, powerful, 0.85).
constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, powerless, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

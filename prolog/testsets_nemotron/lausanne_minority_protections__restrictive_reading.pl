% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections — Restrictive Reading (Individual Worship Only)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The restrictive reading of Lausanne Treaty Articles 37–45 treats minority
 *   protections as limited to individual freedom of worship (Article 38/1:
 *   'free exercise of religion') while categorizing institutional autonomy,
 *   property rights, and theological education as domestic matters governed
 *   by general Turkish law. This reading has been the operational doctrine of
 *   the Turkish state since the 1930s, implemented through the 1935/1936
 *   foundations laws, the 1971 closure of Halki Seminary, the 1974 property
 *   expropriation law, and the narrow 2008/2011 restitution framework. The
 *   constraint is a high-extractiveness snare: minority institutions are the
 *   victims, the state apparatus is the beneficiary, and the coordination
 *   cover (individual worship is permitted) masks the extraction of
 *   institutional capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.87).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.91).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections — Restrictive Reading (Individual Worship Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '71887d21-6ca7-44d0-b6af-584a1b14debc').
narrative_ontology:cs_kernel_codification('71887d21-6ca7-44d0-b6af-584a1b14debc', formalized).
narrative_ontology:cs_authority_grounding('71887d21-6ca7-44d0-b6af-584a1b14debc', extraction).
narrative_ontology:cs_interpretation_layer_present('71887d21-6ca7-44d0-b6af-584a1b14debc').
narrative_ontology:cs_reading_relation('71887d21-6ca7-44d0-b6af-584a1b14debc', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('71887d21-6ca7-44d0-b6af-584a1b14debc', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('71887d21-6ca7-44d0-b6af-584a1b14debc', foundational, minority_protections_exhausted_in_individual_worship).
narrative_ontology:cs_axiom_status(minority_protections_exhausted_in_individual_worship, holdable).
narrative_ontology:cs_axiom_grounding('71887d21-6ca7-44d0-b6af-584a1b14debc', minority_protections_exhausted_in_individual_worship, conventional).
narrative_ontology:cs_axiom('71887d21-6ca7-44d0-b6af-584a1b14debc', foundational, state_sovereignty_absorbs_institutional_guarantees).
narrative_ontology:cs_axiom_status(state_sovereignty_absorbs_institutional_guarantees, holdable).
narrative_ontology:cs_axiom_grounding('71887d21-6ca7-44d0-b6af-584a1b14debc', state_sovereignty_absorbs_institutional_guarantees, conventional).
narrative_ontology:cs_reference_frame('71887d21-6ca7-44d0-b6af-584a1b14debc', state_centric_lausanne_implementation).
narrative_ontology:cs_drift_state('71887d21-6ca7-44d0-b6af-584a1b14debc', contemporary_ecourt_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('71887d21-6ca7-44d0-b6af-584a1b14debc', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_theological_education).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_property_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, individual_minority_worshippers).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, state_sovereignty_over_domestic_law).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, individual_religious_freedom_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the restrictive reading through the Directorate of Religious Affairs (Diyanet), the General Directorate of Foundations (VGM), and the Ministry of National Education. Consolidates control over minority institutional capacity by denying legal personality to religious foundations, restricting property acquisition and inheritance, and subjecting clergy formation to state approval. Collects the institutional assets and decision-making authority that would otherwise belong to autonomous minority structures.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% Greek Orthodox, Armenian Apostolic, and Jewish community foundations (vakıflar) that have been stripped of legal personality since the 1930s and 1970s. Cannot hold title to property, receive bequests, or administer assets independently. Every institutional act — maintaining a church, running a school, electing leadership — requires state permission that is routinely delayed or denied. Exit is identity-locked: the institution IS the community's organized continuity; abandoning it dissolves the community's corporate existence.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_institutions, payer,
    organized, biographical, identity_locked, national).

% The Halki Seminary (Greek Orthodox) closed since 1971; Armenian and Jewish communities have no accredited theological schools. Clergy must be trained abroad (if the state permits exit) or not at all. State refuses to recognize foreign theological degrees for community leadership positions. No domestic alternative exists; the constraint forecloses the reproduction of religious leadership. Exit is trapped — there is no pathway to credentialed clergy within the system, and leaving the country to study often triggers non-return barriers.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_theological_education, payer,
    powerless, generational, trapped, national).

% Community foundations and individual minority members whose properties (churches, schools, cemeteries, revenue-generating assets) were expropriated under Laws 1935, 1936, 1974, and 2008 amendments. The 2008/2011 foundations laws allow some restitution but impose procedural barriers (statutes of limitation, documentary requirements, third-party 'good faith' purchaser protections) that block most claims. Exit is constrained — litigation is possible but costly, slow, and politically sensitive; many claimants are elderly with diminishing capacity to pursue cases.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_property_holders, payer,
    moderate, biographical, constrained, national).

% France, UK, Italy, Japan and other original Lausanne signatories who retain formal guarantor status but have not exercised diplomatic protection for minority institutional rights since the 1930s. Their silence is treated by the state as acquiescence to the restrictive reading. They would object if present (the expansive and guarantor readings are their historical positions) but are structurally excluded from the domestic interpretation process.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, excluded,
    powerful, generational, mobile, continental).

% ECtHR jurisprudence (Bozcaada, Fener Rum Patrikhanesi, Samakov, Hasan and Chaush) establishes that Article 9 ECHR protects collective religious autonomy and property rights — directly contradicting the restrictive reading. Turkey implements judgments narrowly (individual compensation only, no structural remedy). The Court observes but cannot enforce institutional restructuring.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_court_human_rights, observer,
    institutional, generational, analytical, continental).

% Individual believers who retain the right to attend services in functioning churches and synagogues. This is the only Lausanne protection the restrictive reading concedes. They benefit from the limited worship space that exists but have no structural power to defend institutional capacity. Exit is mobile at the individual level — they can worship or not — but the institutional substrate that makes worship possible is eroding beneath them.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, individual_minority_worshippers, beneficiary,
    powerless, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's monopoly over the legal personality and asset base of non-Muslim religious communities, replacing Ottoman millet autonomy with a centralized administrative framework where the state defines the boundaries of permissible religious activity.
% TRANSFER_FUNCTION: Moves institutional assets (real property, endowments, educational infrastructure), decision-making authority (leadership appointment, curriculum, admissions), and legal personality from minority foundations to state agencies (VGM, Diyanet, Ministry of Education). The transfer is one-way: minority institutions lose capacity; the state gains administrative control and asset value.
% ABSENT_VOICES: Guarantor states (France, UK, Italy, Japan) who hold treaty-based standing but have not invoked it for institutional rights since the 1930s. The Ecumenical Patriarchate and Armenian Patriarchate as spiritual heads whose institutional voices are filtered through state-approved leadership. Diaspora communities who fund minority institutions but have no legal standing in Turkish proceedings.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished overnight, minority foundations would regain legal personality, property restitution would proceed without procedural bars, Halki Seminary would reopen, and communities could train clergy and administer assets autonomously. The state would lose its administrative monopoly over minority institutional life. The rearrangement would be structural, not marginal.
% FOUNDING_PROBLEM: The 1923 Treaty of Lausanne was designed to protect non-Muslim minorities in the new Turkish Republic by guaranteeing their religious, educational, and cultural institutions — a response to the Ottoman collapse, population exchanges, and genocide that had destroyed minority communal existence.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — protecting minority institutional continuity — is attested as dead by the Turkish state itself (which declares the minorities 'assimilated' and the treaty satisfied by individual worship), by ECtHR judgments documenting the ongoing institutional foreclosure, and by the minority communities' own demographic collapse (Greek Orthodox from ~200,000 to ~2,000; Armenians from ~2 million to ~60,000). No party outside the state apparatus corroborates that the founding problem is live.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.87, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.87 because the constraint transfers the entire institutional asset base and reproductive capacity of minority communities to the state — property, leadership succession, education, legal personality. Suppression is 0.91 because the constraint persists only through active enforcement: VGM approval for every foundation act, Diyanet control over clergy recognition, Education Ministry veto over theological curricula, judicial barriers to restitution. Theater is 0.28 because the 2008/2011 foundations laws created a restitution process that performs compliance while structurally blocking most claims (deadlines, documentation, third-party protections). Accessibility collapse is 0.38 because alternatives exist in principle (ECtHR litigation, guarantor diplomacy, EU conditionality) but are practically inaccessible to the victim seats. Resistance is 0.62 because communities persist through legal challenges, international advocacy, and demographic survival despite foreclosure.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the restrictive reading is a rope: it coordinates religious diversity within a sovereign legal order, prevents foreign interference, and provides the 'genuine' Lausanne guarantee (individual worship). From the minority institution seats, it is a snare: the coordination cover (worship permitted) masks the extraction of institutional continuity. From the ECtHR seat, it is a treaty violation masquerading as domestic law. The engine computes these divergences from the structural data — the claimed_type 'snare' reflects the analytical seat's assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus (agenda_setter/beneficiary) sits at d≈0.1 — the constraint subsidizes its administrative monopoly and asset control. Minority institutions (payer, identity_locked) sit at d≈0.95 — they bear the full extraction with no exit that preserves institutional identity. Theological education (payer, trapped) sits at d≈1.0 — total foreclosure with no domestic pathway. Property holders (payer, constrained) sit at d≈0.8 — partial recovery possible but structurally impeded. Guarantor states (excluded, mobile) are outside the constraint's directionality but their absence enables it. ECtHR (observer, analytical) sits at d≈0.0 — the constraint does not extract from the Court; the Court's judgments are a structural counter-pressure. Individual worshippers (beneficiary, mobile) sit at d≈0.3 — they receive the conceded worship right but the institutional substrate erodes beneath them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting minority institutional continuity after Ottoman collapse) is dead — the communities have been demographically decimated and their institutions hollowed out. Yet the arrangement persists and intensifies (extractiveness rising from 0.35 to 0.87). The mandate has atrophied into a pure extraction mechanism: the state no longer 'protects' minorities; it administers their disappearance. The restrictive reading is the cover story that legitimates the administration of disappearance as 'domestic law.' This is mandatrophy resolved — the constraint's function has inverted from protection to foreclosure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the restrictive reading a defensible interpretation of the Lausanne text, or a post-hoc construction that forecloses the treaty''s institutional guarantees?',
    'Travaux préparatoires analysis of the 1923 negotiations; comparative treaty interpretation by the ILC; the ICJ''s advisory jurisdiction on treaty interpretation.',
    'If the restrictive reading is textually indefensible, the constraint is a pure snare with no coordination function. If it has textual support, the extraction is layered onto a genuine ambiguity — tangled_rope from the analytical seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the restrictive reading''s core premise (individual worship only) is a plausible treaty interpretation or a constructed foreclosure of institutional guarantees.').

omega_variable(
    guarantor_state_acquiescence,
    'Does the prolonged silence of guarantor states constitute legal acquiescence to the restrictive reading, or is it a political choice that leaves the treaty obligation intact?',
    'State practice and opinio juris analysis; ILC draft articles on state responsibility for breach of treaty obligations; the VCLT framework on treaty termination and suspension.',
    'If acquiescence, the restrictive reading gains legal legitimacy and the snare hardens into a mountain-like ''settled law.'' If not, the extraction remains a continuing breach with remedial consequences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guarantor_state_acquiescence, conceptual, 'Whether guarantor state inaction legally validates the restrictive reading or merely reflects political abandonment of the minority populations.').

omega_variable(
    demographic_extinction_threshold,
    'At what demographic threshold does the restrictive reading''s extraction become moot because the victim communities have effectively vanished?',
    'Demographic projection modeling; legal analysis of treaty obligations erga omnes partes vs. obligations dependent on beneficiary existence; the ICJ''s jurisprudence on the extinction of treaty rights.',
    'If extraction continues past demographic extinction, the constraint reveals itself as structural elimination rather than regulation. The snare classification would persist but the victim set would shift from living institutions to historical memory and diaspora claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_extinction_threshold, empirical, 'Whether the constraint''s extractive logic self-terminates when the victim populations fall below functional viability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.08).
narrative_ontology:measurement(laus_tr_t1935, lausanne_minority_protections__restrictive_reading, theater_ratio, 1935, 0.15).
narrative_ontology:measurement(laus_tr_t1942, lausanne_minority_protections__restrictive_reading, theater_ratio, 1942, 0.22).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__restrictive_reading, theater_ratio, 1971, 0.25).
narrative_ontology:measurement(laus_tr_t2008, lausanne_minority_protections__restrictive_reading, theater_ratio, 2008, 0.27).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement(laus_be_t1935, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1935, 0.52).
narrative_ontology:measurement(laus_be_t1942, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1942, 0.68).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1971, 0.79).
narrative_ontology:measurement(laus_be_t2008, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.4).
narrative_ontology:measurement(laus_su_t1935, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1935, 0.65).
narrative_ontology:measurement(laus_su_t1942, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1942, 0.78).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1971, 0.85).
narrative_ontology:measurement(laus_su_t2008, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2008, 0.89).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__restrictive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, ecourt_article9_collective_autonomy).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, turkish_foundations_law_2008_restitution).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate_legal_personality).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, halki_seminary_closure).

% DUAL FORMULATION NOTE:
% Part of the lausanne_minority_protections constraint family. This restrictive_reading extracts at ε=0.87 from minority institutions. The expansive_reading (ε≈0.35) coordinates institutional continuity; the guarantor_reading (ε≈0.15) coordinates international supervision. The three readings share the same treaty text but instantiate structurally distinct constraints with different ε, victims, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__restrictive_reading, institutional, 0.1).
constraint_indexing:directionality_override(lausanne_minority_protections__restrictive_reading, organized, 0.95).
constraint_indexing:directionality_override(lausanne_minority_protections__restrictive_reading, powerless, 1.0).
constraint_indexing:directionality_override(lausanne_minority_protections__restrictive_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

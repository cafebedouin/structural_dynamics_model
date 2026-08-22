% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Communal Authority Over Marriage and Divorce
 *   domain: constitutional/religious governance/family law
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act 1936 codifies ancient Zoroastrian
 *   custom into Indian statutory law, granting Parsi community tribunals
 *   exclusive authority over marriage validity, divorce, and inheritance for
 *   Parsi citizens. The constraint is read by the Parsi communal authority as
 *   genuine coordination (marriage governance and cultural continuity), and
 *   by civil rights advocates as identity-lock extraction (endogamy
 *   enforcement that forecloses exit). This story instantiates the Parsi
 *   communal reading: the constraint derives its legitimacy from community
 *   custom as codified in the Act; community tribunals are the authority
 *   structure; endogamy enforcement is the boundary-maintenance mechanism.
 *   The constraint is one of five competing readings of the marriage
 *   authority kernel, each grounded in a different epistemic and normative
 *   framework (Zoroastrian custom, Hindu codification, Islamic shariat,
 *   Christian canon, secular individual rights). The structural delta for
 *   this reading is: community tribunal arbitration, high gender equity
 *   within the Act's provisions, endogamy lock on exit, and demographic
 *   decline as a background existential pressure.
 *
 * KEY AGENTS:
 *   - Parsi community tribunals (panchayats, high priests' council): agenda-setter, organized power, enforce marriage rules and endogamy
 *   - Parsi women: identity-locked beneficiaries and payers, moderate power, gain equitable divorce rights but forfeit exogamous marriage
 *   - Parsi men: identity-locked beneficiaries and payers, moderate power, asymmetrically positioned vis-à-vis endogamy enforcement
 *   - Parsi diaspora (India, overseas): payers, powerful but constrained by portable identity lock
 *   - Indian secular courts: excluded observer, institutional power, barred from full jurisdiction by personal law exemption
 *   - Hindu, Muslim, Christian community authorities: observer competitors for constitutional recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.38).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Authority Over Marriage and Divorce").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "constitutional/religious governance/family law").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '4da2aaca-b138-446c-b9f7-f985b71c85f3').
narrative_ontology:cs_kernel_codification('4da2aaca-b138-446c-b9f7-f985b71c85f3', fixed_text).
narrative_ontology:cs_authority_grounding('4da2aaca-b138-446c-b9f7-f985b71c85f3', lineage).
narrative_ontology:cs_interpretation_layer_present('4da2aaca-b138-446c-b9f7-f985b71c85f3').
narrative_ontology:cs_reading_relation('4da2aaca-b138-446c-b9f7-f985b71c85f3', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('4da2aaca-b138-446c-b9f7-f985b71c85f3', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4da2aaca-b138-446c-b9f7-f985b71c85f3', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4da2aaca-b138-446c-b9f7-f985b71c85f3', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('4da2aaca-b138-446c-b9f7-f985b71c85f3', foundational, community_custom_is_legitimate_law).
narrative_ontology:cs_axiom_status(community_custom_is_legitimate_law, holdable).
narrative_ontology:cs_axiom_grounding('4da2aaca-b138-446c-b9f7-f985b71c85f3', community_custom_is_legitimate_law, conventional).
narrative_ontology:cs_axiom('4da2aaca-b138-446c-b9f7-f985b71c85f3', foundational, endogamy_is_necessary_for_community_continuity).
narrative_ontology:cs_axiom_status(endogamy_is_necessary_for_community_continuity, holdable).
narrative_ontology:cs_axiom_grounding('4da2aaca-b138-446c-b9f7-f985b71c85f3', endogamy_is_necessary_for_community_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('4da2aaca-b138-446c-b9f7-f985b71c85f3', zoroastrian_custom_transmitted_through_lineage).
narrative_ontology:cs_drift_state('4da2aaca-b138-446c-b9f7-f985b71c85f3', post_secular_constitutional_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4da2aaca-b138-446c-b9f7-f985b71c85f3', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_continuity).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, community_tribunal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_women).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_men).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_men).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_diaspora_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parsi marriage courts (Panchayats and the High Priest's council) interpret and enforce the 1936 Act, adjudicate disputes over validity, register marriages, and oversee divorce proceedings. They claim authority rooted in ancient Zoroastrian custom as codified in the Act. Their legitimacy derives from community recognition and the statutory delegation under Indian law, which exempts them from full civil court oversight. They maintain genealogies, enforce endogamy rules, and certify eligibility for marriage.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_tribunals, agenda_setter,
    organized, generational, constrained, national).

% Possess substantial property rights and divorce rights under the 1936 Act compared to Hindu and Muslim personal law alternatives—they can initiate divorce on grounds including cruelty and desertion with relative ease. They also bear the constraint that marriage outside the community forfeits community status and inheritance rights, locking them into the endogamy rule. Their identity as Parsi is constituted through kinship and community membership; exit from the communal authority structure means exit from the community itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_women, payer).

% Also benefit from relatively gender-equitable divorce rules and inherit the authority structure as legitimate. They also face the endogamy constraint and identity lock. In practice, men hold slightly more exit flexibility because interfaith marriages by men do not trigger automatic community loss in the same way; the constraint is asymmetrically distributed along gender lines.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_men, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_men, payer).

% Parsis living abroad or in mixed-religion relationships face enforcement of endogamy and exclusion from community recognition if they marry outside. The constraint's reach extends globally via diaspora networks; those who exit the endogamy rule lose inheritance, temple access, and family standing. Geographic mobility does not provide meaningful exit because the identity lock is portable.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_diaspora_members, payer,
    powerful, biographical, constrained, global).

% Are formally excluded from full jurisdiction over Parsi matrimonial matters by the exemption structure built into Indian constitutional pluralism. They can review decisions for gross procedural violation or constitutional violation, but cannot substitute their judgment on matters of personal law without overriding the communal authority delegation. They would argue for uniform civil code application but are structurally barred by the personal-law exemption.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_civil_courts, excluded,
    institutional, generational, constrained, national).

% Operate under parallel personal law regimes with their own community tribunals and statutory authority structures. They observe Parsi communal authority and compete for political recognition and legal deference. The Parsi reading establishes a precedent for community self-governance that other religious minorities leverage.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, hindu_muslim_christian_communities, observer,
    organized, generational, analytical, national).

% Enacted the 1936 Act and embedded the Parsi exemption in the constitutional architecture of personal law pluralism. Parliament retains theoretical power to abrogate the exemption and impose uniform civil code, but doing so would violate the implicit accommodation with minority religious communities that underpins Indian constitutional legitimacy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_state_parliament, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: Parsi community survival and cultural continuity in diaspora following migration from Persia (7th–8th centuries) and settlement in India. A small, ethnoreligious minority faced dissolution through assimilation and intermarriage absent institutional mechanisms to enforce group boundaries and genealogical continuity.
% FOUNDING_PROBLEM_CORROBORATION: Parsi community authorities and religious scholars attest the founding problem remains live. Census data and demographic analysis from outside the community (academic demography, sociological research) confirm the Parsi population is declining and increasingly intermarried. However, civil rights advocates and secular constitutional scholars argue the founding problem is either overstated (voluntary cultural participation could sustain the community) or already functionally dead (the constraint merely delays, not prevents, assimilation). No corroboration exists from civil society sources outside the community that the constraint is necessary for continuity; the corroboration from within the community is direct self-interest.
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).
:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint genuinely solves a coordination problem—communal marriage authority reduces friction and preserves genealogical continuity—but the endogamy rule imposes a specific cost: identity-locked agents cannot exit without losing community status, and the constraint blocks the exit-option door by making marriage outside the community a self-exile. Suppression is at 0.52: the constraint is enforced through legal penalty (denial of inheritance, temple access, genealogical recognition), but suppression is not as severe as a snare because within-community marriage is voluntary and the rules are transparent. Theater ratio is moderate (0.28) and rising toward 2005: as demographic decline and intermarriage increase, more of the constraint's activity is devoted to defending the endogamy boundary (boundary patrol, genealogical exclusion) rather than solving the original coordination problem. The ratio has declined slightly by 2026 (to 0.28) because the constraint's actual scope has shrunk—fewer Parsis remain to police or maintain it, so the performative aspect has stabilized at a lower intensity. Suppression requirement rose from 1936 to 2005 (enforcing endogamy against increasing exogamous pressure) and declined slightly by 2026 (enforcement becomes harder as the community shrinks). The measurement series is authored on one shared time grid (1936, 1960, 1985, 2005, 2020, 2026) to avoid metric misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the Parsi community tribunal seat, the constraint is a rope: it coordinates marriage authority, preserves custom, and enables gender-equitable dispute resolution within a recognized framework. From the identity-locked beneficiary seat (Parsi women), the constraint is experienced as partially extractive: the coordination benefit is real, but the endogamy lock forecloses marriage choice and ties community status to genealogical approval. From the secular court seat, the constraint appears as an unjustified exemption—a snare mechanism dressed as cultural continuity. The engine computes these seat-divergent classifications from the structural data (power, exit options, beneficiary/victim roles); the authored claim (rope) reflects the communal reading, not the secular court reading or the identity-locked beneficiary perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The parsi_community_tribunals hold a d near 0.0 (beneficiary: they set the rules, collect authority, face no exit pressure). Parsi women and men both hold d near 0.5 (symmetric): they benefit from communal coordination and equitable divorce rules, but they pay the endogamy cost and face identity lock. The identity_locked exit option is the key: it means that while the constraint nominally permits exit (one can marry outside), exiting incurs the penalty of losing Parsi identity, which is constitutive of the agent's self-conception. This drives d upward toward the target end because the exit cost is not just economic but existential. Parsi diaspora members hold d near 0.6 (tilted target): they face the constraint's suppression (enforcement of endogamy despite global dispersion) without the close community ties that make within-group marriage naturally attractive; they experience the constraint as an identity requirement imposed from afar. Secular civil courts and excluded community authorities hold d near 0.5 (analytical); they are not directly subject to the constraint's extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communal continuity in diaspora) is contested: the community tribunals attest it is live, but secular authorities and civil rights advocates attest it is either overstated (voluntary cultural participation could sustain the community without coercive endogamy) or already dead (the Parsi population is declining regardless, and the constraint merely delays rather than prevents assimilation). The constraint's classification hinges on this dispute. If the founding problem is live and cannot be solved by voluntary coordination alone, the constraint is a tangled rope (coordination + extraction). If the founding problem is dead or can be solved without endogamy enforcement, the constraint is a snare (extraction dressed as cultural continuity). The authored claim (rope) reflects the communal reading; a secular reading would claim snare. The engine's computation will diverge from the claim where the metrics are inconsistent with pure coordination—the rising theater_ratio and suppression_requirement are signals that the constraint is doing more boundary-defense work than coordination work, which supports a tangled rope or snare reclassification from the secular seat. The mandatrophy question is: does the community need the constraint's enforcement infrastructure to survive, or does the constraint survive by preventing the community from making a genuine choice about its continuity? The contested founding_problem_status flags this as unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_continuity_vs_coercive_boundary,
    'Can the Parsi community maintain cultural and genealogical continuity without coercive endogamy enforcement, or does the continuity necessarily depend on restricting the exit option?',
    'Comparative analysis of diaspora communities that have abandoned endogamy enforcement while retaining cultural practices (e.g., Jewish diaspora variants, Armenian diaspora patterns); longitudinal demographic modeling of Parsi population under counterfactual voluntary participation; ethnographic research on what aspects of Parsi identity require kinship-based enforcement versus what can survive voluntary cultural practice.',
    'If continuity is achievable without enforcement, the constraint reclassifies from rope/tangled rope to snare: the endogamy rule would be revealed as extraction disguised as cultural necessity. If continuity requires enforcement, the constraint remains rope or tangled rope depending on whether the gender-equitable divorce rules offset the identity lock for agent valuations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_continuity_vs_coercive_boundary, empirical, 'Whether communal continuity requires or is merely serviced by endogamy enforcement.').

omega_variable(
    identity_lock_internalization,
    'Is the identity lock that binds Parsi agents to the constraint a structural property (genealogical exclusion is institutionally imposed) or an internalized psychological commitment (agents have fused their self-concept with Parsi communal membership)?',
    'Post-exit trajectory analysis: if Parsi agents who marry outside the community and forfeit formal status report persistent identity distress and continued deference to community norms, the lock is partly internalized. If they report relief and swift reorientation, the lock is primarily structural.',
    'If internalized, the effective suppression is higher than the 0.52 metric suggests—the agent carries the constraint''s logic with them after exit. If structural, removing the enforcement mechanism would allow exit. The classification implication is toward higher extraction (snare-adjacent) if internalized, toward pure enforcement cost if structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'The identity lock''s structural versus internalized character.').

omega_variable(
    constitutional_pluralism_vs_uniformity,
    'Is the personal law exemption that underpins Parsi communal authority structurally compatible with constitutional principles of equality and individual rights, or does it instantiate a coercive carve-out that should be subjected to uniform civil code application?',
    'Constitutional court precedent and legislative amendment; comparative analysis of whether personal law pluralism produces net better outcomes for minority communities (lower conflict, preserved cultural autonomy) or net worse (embedded extraction, identity lock, gender discrimination).',
    'If constitutional pluralism is justified, the constraint remains a legitimate rope/tangled rope under the communal reading. If it is unjustified, the constraint becomes a snare maintained through state deference to religious authority. This is the foundational frame-choice that determines whether the communal reading or the secular civil reading prevails in classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_pluralism_vs_uniformity, conceptual, 'The legitimacy of constitutional personal law pluralism as a framework for authority.').

omega_variable(
    demographic_decline_causation,
    'Is the Parsi population decline caused by the constraint (endogamy prevents population growth and forces assimilation through marriage exit), or does the constraint merely fail to prevent a decline driven by other factors (economic migration, lower fertility rates, delayed marriage)?',
    'Controlled comparison of intermarriage rates among Parsis who live under community tribunal enforcement versus diaspora Parsis in jurisdictions where enforcement is unavailable; demographic decomposition of fertility versus migration versus intermarriage contributions to population change.',
    'If the constraint causes decline, it is partly self-defeating: a zombie constraint maintaining an identity lock for a community that no longer exists. If decline is exogenous, the constraint persists as coordination mechanism for a shrinking population and becomes increasingly theatrical as the coordination problem itself shrinks. The theater_ratio rise through 2005 and decline by 2026 is consistent with the latter interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_decline_causation, empirical, 'Whether demographic decline is endogenous to the constraint or exogenous.').

omega_variable(
    sibling_reading_frame_choice,
    'Which reading of the marriage authority kernel is authoritative: the Parsi communal reading grounding authority in custom, the secular civil reading grounding authority in constitutional equality, or one of the other religious readings?',
    'This is not empirically resolvable; it is the frame-choice that determines what counts as a legitimate authority structure. The Indian constitutional accommodation of personal law pluralism currently privileges the communal readings (Parsi, Hindu, Muslim, Christian) over the secular civil reading, but that accommodation is itself contested and subject to reform.',
    'If the communal reading is authoritative, the constraint is rope/tangled rope from the communal seat and may be snare from the secular seat (seat-divergent classification). If the secular reading is authoritative, the constraint is snare from every seat. This omega captures the irreducible disagreement about legitimacy that cannot be resolved by metric measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_frame_choice, preference, 'The frame-choice that determines which reading is authoritative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.12).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(marr_tr_t2020, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.32).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(marr_be_t2020, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.38).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(marr_su_t2020, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__parsi_communal_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the marriage authority kernel family. All five readings (Parsi communal, Hindu codified, Muslim shariat, Christian canonical, secular civil) describe the same institutional domain (marriage authority in India) but ground legitimacy in different epistemic and normative frameworks. Each reading instantiates a different constraint with different ε values and classifications. The sibling readings coexist as competing positions held by different institutional actors (community tribunals, civil courts, constitutional frameworks). No single reading forecloses another within the pluralistic Indian constitutional architecture, though each reading competes for state recognition and legitimacy. Decomposition rationale: the Parsi reading is structurally distinct because it grounds authority in Zoroastrian custom codification and community tribunal arbitration, creating a unique nexus of identity lock (endogamy enforcement), gender equity (within the Act), and demographic pressure (community decline). Other readings ground authority differently (statutory codification with civil court review for Hindu, Islamic jurisprudential interpretation for Muslim, canonical tradition for Christian, individual constitutional rights for secular) and produce different victim sets and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

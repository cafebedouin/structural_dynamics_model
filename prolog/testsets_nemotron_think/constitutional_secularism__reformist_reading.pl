% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: State Affirmative Duty to Eliminate Religious Practices Oppressing Marginalized Groups
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The reformist reading of constitutional secularism holds that the state
 *   has an affirmative, non-derogable duty to eliminate religious practices
 *   that oppress marginalized groups — scheduled castes and women — even when
 *   this supersedes claims of religious autonomy. This reading treats
 *   Articles 17 (abolition of untouchability), 25(2)(b) (state power to
 *   regulate secular activity associated with religion for social
 *   welfare/reform), and 44 (uniform civil code directive) as a unified
 *   mandate for transformative intervention. The constraint is the most
 *   extractive on religious autonomy among the three kernel readings: it does
 *   not merely permit intervention (principled_intervention) or demand
 *   neutrality (strict_neutrality); it requires intervention as a positive
 *   duty. Beneficiaries are structurally oppressed groups who cannot achieve
 *   liberation through internal reform; victims are religious conservatives
 *   and institutions whose authority over practice is displaced by state
 *   mandate. The constraint requires active enforcement (legislation,
 *   judicial orders, police power) and has no sunset clause — the duty
 *   persists as long as oppressive practices exist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.82).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.78).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "State Affirmative Duty to Eliminate Religious Practices Oppressing Marginalized Groups").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '008b66b9-3478-4aeb-bdb3-4594eeb7acf2').
narrative_ontology:cs_kernel_codification('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', formalized).
narrative_ontology:cs_authority_grounding('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', extraction).
narrative_ontology:cs_interpretation_layer_present('008b66b9-3478-4aeb-bdb3-4594eeb7acf2').
narrative_ontology:cs_reading_relation('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_axiom('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', foundational, state_affirmative_duty_eliminate_religious_oppression).
narrative_ontology:cs_axiom_status(state_affirmative_duty_eliminate_religious_oppression, holdable).
narrative_ontology:cs_axiom_grounding('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', state_affirmative_duty_eliminate_religious_oppression, deontological).
narrative_ontology:cs_axiom('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', foundational, religious_autonomy_subordinate_to_substantive_equality).
narrative_ontology:cs_axiom_status(religious_autonomy_subordinate_to_substantive_equality, holdable).
narrative_ontology:cs_axiom_grounding('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', religious_autonomy_subordinate_to_substantive_equality, deontological).
narrative_ontology:cs_axiom('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', secondary, internal_reform_insufficient_for_structural_oppression).
narrative_ontology:cs_axiom_status(internal_reform_insufficient_for_structural_oppression, holdable).
narrative_ontology:cs_axiom_grounding('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', internal_reform_insufficient_for_structural_oppression, empirically_contingent).
narrative_ontology:cs_reference_frame('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', constitutional_transformative_equality).
narrative_ontology:cs_drift_state('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', contemporary_judicial_majoritarianism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('008b66b9-3478-4aeb-bdb3-4594eeb7acf2', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, marginalized_women).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, secular_reformers).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, transformative_constitutionalism).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, substantive_equality_over_formal_neutrality).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, state_as_emancipator).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces legislation eliminating religious practices deemed oppressive (e.g., anti-caste discrimination laws, bans on triple talaq, temple entry legislation). Claims mandate from constitutional transformative equality. Controls enforcement machinery and defines the scope of oppressive practices.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicates conflicts between religious autonomy claims and state reform legislation. Expands or contracts the reformist reading through constitutional interpretation (e.g., Sabarimala, triple talaq judgments). Not a direct beneficiary but wields authoritative interpretation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, judiciary, observer).

% Historically excluded from temples, wells, public spaces by religiously-sanctioned caste practice. State intervention enables access and dignity. Exit from caste oppression is structurally constrained — cannot individually opt out of caste hierarchy; depend on state enforcement for meaningful change.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    organized, biographical, constrained, national).

% Subject to religious personal laws governing marriage, divorce, inheritance that discriminate on gender (e.g., triple talaq, unequal inheritance, denial of ritual participation). State reform legislation provides legal exit from discriminatory rules. Individual exit from community enforcement is constrained by social and economic dependence.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, marginalized_women, beneficiary,
    organized, biographical, constrained, national).

% View state intervention as existential threat to religious identity and communal autonomy. Practices targeted (caste exclusion, gendered personal laws, ritual restrictions) are experienced as core religious obligations, not optional customs. Exit from the constraint would require abandoning religious self-understanding — identity-locked. Bear costs of lost authority, legal penalties, social marginalization when resisting.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives, payer,
    organized, biographical, identity_locked, national).

% Temples, mosques, churches, personal law boards lose control over membership, ritual, and internal governance. State mandates override institutional authority (e.g., temple entry, clergy appointment, marriage dissolution). Can litigate but cannot exit the state's jurisdiction; constrained exit within national legal order.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_institutions, payer,
    organized, generational, constrained, national).

% Civil society actors, lawyers, academics who advocate for state intervention against religious oppression. Gain ideological validation and policy influence when reformist reading prevails. Mobile exit — can shift advocacy focus; not structurally dependent on this constraint.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, secular_reformers, beneficiary,
    moderate, biographical, mobile, national).

% Activists within religious communities seeking internal reform (e.g., Muslim women against triple talaq, Dalit priests demanding temple access). Support state intervention strategically but prefer community-led transformation. Excluded from the binary framing of state vs. religion; their voice complicates the reformist narrative.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_minority_reformers, excluded,
    moderate, biographical, constrained, national).

% Monitor state compliance with CEDAW, ICERD, ICCPR regarding religious discrimination. Provide external legitimacy to reformist reading but lack enforcement power. Analytical seat — observe and report, neither collect nor pay.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of internal reform within religious communities: oppressed members cannot individually overthrow practices sanctioned by communal authority; state provides external enforcement lever to break coordination failure among the oppressed and override veto power of dominant groups within communities.
% TRANSFER_FUNCTION: Moves religious authority over practices affecting marginalized groups from religious institutions/conservative elites to the state; moves legal protection, dignity, and access from denial to realization for scheduled castes and marginalized women. The transfer is asymmetric: religious conservatives lose unchallengeable control; marginalized groups gain enforceable rights.
% ABSENT_VOICES: Religious minority reformers who support the substantive goal (ending caste/gender oppression) but reject state-led imposition as colonial/majoritarian overreach — they are excluded because the reformist reading frames the choice as state intervention vs. status quo, erasing the third option of supported internal reform. Also absent: marginalized members of majority religious communities (e.g., Dalit Hindus) who experience the reformist reading as selectively targeting minority personal laws while majority community practices persist.
% DISAPPEARANCE_RATIONALE: If the state's affirmative duty vanished overnight, religiously-sanctioned caste exclusion (temple entry, access to public resources), gender-discriminatory personal laws (triple talaq, unequal inheritance, marital rape exemption), and ritual restrictions would revert to community control without external check. Marginalized groups would lose the only enforceable lever against practices their own communities enforce. The legal architecture of Articles 17, 25(2)(b), 44 would become aspirational rather than operative.
% FOUNDING_PROBLEM: Historical and ongoing oppression of scheduled castes and women through religiously-sanctioned practices: untouchability and temple exclusion justified by purity doctrines; gender-discriminatory personal laws (triple talaq, polygamy, unequal inheritance, denial of divorce) justified by scriptural interpretation; denial of agency in religious leadership and ritual participation. The founding problem is that internal community reform consistently fails because dominant groups within communities control interpretive authority and material enforcement.
% FOUNDING_PROBLEM_CORROBORATION: B.R. Ambedkar (Constituent Assembly debates, Annihilation of Caste) attested the founding problem is live — caste oppression is religiously structured and requires state destruction. Feminist scholars (Flavia Agnes, Vrinda Grover) corroborate from outside the state-beneficiary set: personal law reform remains incomplete and state intervention is necessary but insufficient. Human Rights Watch and UN CEDAW Committee reports document ongoing religiously-sanctioned discrimination. Counter-corroboration: some religious minority leaders argue the founding problem is substantially solved (caste discrimination legally abolished, triple talaq banned) and continued intervention is majoritarian policing.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint transfers authority over core religious practices (marriage, ritual, membership) from communities to state, and this transfer is enforced coercively. Suppression (0.78) is high because alternatives — internal reform, community-led change, exit from community — are structurally blocked or delegitimized by the reformist frame. Theater ratio (0.35) is moderate: the coordination function (protecting the oppressed) is genuine and empirically verified (temple entry, triple talaq ban, anti-untouchability enforcement have materially improved lives), but a growing share of enforcement energy targets practices where the oppression claim is contested (e.g., Sabarimala, hijab bans framed as reform). Accessibility collapse (0.68) reflects that once the state declares a practice oppressive, the space for community self-correction collapses — the practice becomes legally prohibited, not negotiable. Resistance (0.75) is high: religious conservatives mobilize legally, politically, and socially against each intervention, framing it as existential threat to religious identity.
 *
 * PERSPECTIVAL GAP:
 *   From the state/judiciary seat (agenda_setter, institutional, arbitrage exit), the constraint appears as rope/tangled_rope: genuine coordination solving a collective-action failure of internal reform, with extraction as necessary cost. From the religious conservative seat (payer, organized, identity_locked), it appears as snare: the coordination story is experienced as cover for majoritarian imposition; suppression is internalized as religious persecution. From the marginalized beneficiary seats (beneficiary, organized, constrained exit), it appears as rope: the only structure that delivers enforceable rights. The engine computes this divergence from structural data — the claimed_type (tangled_rope) is the author's structural assessment; per-seat computation will differ.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislature and judiciary are agenda_setters with institutional power and arbitrage-grade exit (they design and administer the constraint). Scheduled castes and marginalized women are beneficiaries with organized power but constrained exit — they cannot individually escape caste/gender oppression; the constraint is their primary lever. Religious conservatives are payers with organized power but identity_locked exit — their religious self-understanding fuses with the targeted practices; leaving the constraint means leaving their identity. Religious institutions are payers with organized power and constrained exit — they operate within state jurisdiction but lose governance authority. Secular reformers are beneficiaries with moderate power and mobile exit. Religious minority reformers are excluded — their position (reform yes, state imposition no) is structurally unrepresented in the binary. International bodies are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religiously-sanctioned oppression of castes and women) is contested as live vs. substantially solved. If dead, the constraint persists as mandatrophy — an affirmative duty that has outlived its function, maintained by institutional inertia and ideological commitment. The reformist reading resists mandatrophy resolution by expanding the definition of 'oppressive practice' (new targets: Sabarimala, hijab, religious conversions) — mission creep sustains the constraint. The mandatrophy analysis must distinguish: (a) practices where oppression is empirically verified and internal reform has failed (caste exclusion, triple talaq) — constraint remains coordination; (b) practices where oppression is contested and internal reformers exist (Sabarimala, religious dress) — constraint drifts toward extraction. The single claimed_type cannot capture this internal differentiation; the omega on coordination-extraction boundary addresses it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the reformist reading''s classification change if the kernel (constitutional secularism) is framed as a single constraint with observable-dependent classification versus three distinct constraints (three readings) with independent ε values?',
    'Apply the ε-invariance test: measure extractiveness of the state''s secularism obligation under each reading''s operationalization. If ε differs substantially across readings (reformist: high; strict_neutrality: near-zero; principled_intervention: moderate), they are distinct constraints per DP-001. The kernel context is authoring metadata, not a structural parameter.',
    'If readings are distinct constraints, each gets its own classification, stakeholders, and temporal trajectory. The reformist reading''s high extractiveness is not ''balanced'' by the strict_neutrality reading''s low extractiveness. False summation across readings is prevented.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the constitutional secularism kernel decomposes into multiple ε-invariant constraints or one constraint with reading-dependent classification.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (protecting marginalized groups from practices they cannot internally reform) end and asymmetric extraction (displacing religious authority over contested practices where internal reformers exist) begin?',
    'Case-by-case empirical assessment: for each targeted practice, (1) is there documented failure of internal reform over decades? (2) do marginalized members of the community support state intervention? (3) is the practice central to religious identity such that suppression generates identity-locked resistance? Practices scoring high on all three are extraction-dominant; practices scoring high on (1) and (2) but low on (3) are coordination-dominant.',
    'If the boundary is permeable, the constraint''s claimed_type (tangled_rope) is unstable — it may be a constraint family where some sub-constraints are rope (caste exclusion) and others snare (Sabarimala). Decomposition would be required per ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the reformist reading''s coordination and extraction components are separable by practice, requiring constraint decomposition.').

omega_variable(
    suppression_mechanism_religious_conservatives,
    'Is the high suppression experienced by religious conservatives structural (legal penalties, state enforcement) or internalized (identity fusion making the constraint feel like persecution even without active enforcement)?',
    'Post-intervention suppression trajectory: track religious conservative resistance after judicial validation of reform (e.g., post-Sabarimala, post-triple talaq ban). If resistance persists at high intensity despite settled law and reduced active enforcement, reclassify as partially internalized — the constraint has become identity-constitutive.',
    'If substantially internalized, effective suppression is higher than structural measure suggests; the constraint extracts identity-costs beyond legal compliance. This amplifies the snare-character for the conservative seat and may shift per-seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_religious_conservatives, empirical, 'Structural vs. internalized suppression mechanism for identity-locked religious conservative payers.').

omega_variable(
    selective_targeting_majority_vs_minority_practices,
    'Does the reformist reading selectively target minority religious practices (Muslim personal law, Christian inheritance) while leaving majority community practices (Hindu caste endogamy, temple management) relatively untouched, and if so, does this selectivity constitute extraction on a communal axis?',
    'Comparative enumeration of legislative/judicial interventions 1950-2024 by religious community targeted. Measure: (a) number of interventions, (b) depth of intrusion into core practice, (c) community perception of targeting. If minority communities bear disproportionate intrusion depth per oppressive practice, the constraint has a communal extraction vector.',
    'If selective targeting is confirmed, the constraint is not a neutral tangled_rope but a communal snare — extraction falls asymmetrically on minority religious conservatives while majority conservatives are comparatively spared. This would require a separate constraint story for the communal extraction axis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_targeting_majority_vs_minority_practices, empirical, 'Whether the reformist reading''s enforcement pattern extracts disproportionately from minority religious communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cs_reformist_tr_t1950, constitutional_secularism__reformist_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(cs_reformist_tr_t1965, constitutional_secularism__reformist_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(cs_reformist_tr_t1985, constitutional_secularism__reformist_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(cs_reformist_tr_t1995, constitutional_secularism__reformist_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(cs_reformist_tr_t2005, constitutional_secularism__reformist_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(cs_reformist_tr_t2015, constitutional_secularism__reformist_reading, theater_ratio, 2015, 0.34).
narrative_ontology:measurement(cs_reformist_tr_t2024, constitutional_secularism__reformist_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(cs_reformist_be_t1950, constitutional_secularism__reformist_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(cs_reformist_be_t1965, constitutional_secularism__reformist_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(cs_reformist_be_t1985, constitutional_secularism__reformist_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(cs_reformist_be_t1995, constitutional_secularism__reformist_reading, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement(cs_reformist_be_t2005, constitutional_secularism__reformist_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(cs_reformist_be_t2015, constitutional_secularism__reformist_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(cs_reformist_be_t2024, constitutional_secularism__reformist_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(cs_reformist_su_t1950, constitutional_secularism__reformist_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(cs_reformist_su_t1965, constitutional_secularism__reformist_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(cs_reformist_su_t1985, constitutional_secularism__reformist_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(cs_reformist_su_t1995, constitutional_secularism__reformist_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(cs_reformist_su_t2005, constitutional_secularism__reformist_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(cs_reformist_su_t2015, constitutional_secularism__reformist_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(cs_reformist_su_t2024, constitutional_secularism__reformist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__reformist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

% DUAL FORMULATION NOTE:
% The constitutional_secularism kernel decomposes into three readings with divergent ε: strict_neutrality_reading (ε≈0.05, Mountain) — state non-interference as coordination standard; principled_intervention_reading (ε≈0.45, Tangled Rope) — permissive intervention for social reform; reformist_reading (ε≈0.82, Tangled Rope) — affirmative duty to eliminate oppression. The reformist reading forecloses strict_neutrality (affirmative duty vs. non-interference are logically incompatible in one framework) and coexists_with principled_intervention (both permit intervention; reformist demands it). Upstream: strict_neutrality provides the baseline from which intervention readings deviate. Downstream: reformist reading's expansion of 'oppressive practice' creates pressure on principled_intervention to justify its permissive threshold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, organized, 0.85).
constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

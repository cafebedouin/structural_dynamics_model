% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Secular Civil-Contract Reading of Marriage Authority
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story authors the secular contractual reading of marriage as its own
 *   constraint: a civil registration procedure grounded in state authority
 *   rather than religious sanction, available to any two competent adults
 *   regardless of faith. It is one of five sibling readings of a single
 *   kernel — the question of who has legitimate authority to declare a
 *   marriage valid. This reading's structural signature is registration as
 *   the sole validity criterion, gender-symmetric default rights, and
 *   interfaith/non-religious accessibility. It does not describe or average
 *   over the sacramental, dharmashastra, shariat, or Zoroastrian readings;
 *   each of those is a separate constraint with its own ε.
 *
 * KEY AGENTS:
 *   - civil_registry_state: agenda_setter (institutional/analytical) — administers registration as sole validity test
 *   - interfaith_couples: beneficiary (moderate/mobile) — gain a marriage route no personal-law regime offers
 *   - women_seeking_gender_symmetric_rights: beneficiary (moderate/constrained) — gain formal symmetry, not necessarily practical parity
 *   - religious_authorities_losing_jurisdiction: payer (organized/constrained) — lose legal monopoly on validity
 *   - community_elders_enforcing_endogamy: payer (organized/constrained) — lose enforcement leverage over communal marriage norms
 *   - judiciary_constitutional_courts: observer (institutional/analytical) — adjudicates boundary disputes between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.28).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.22).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Secular Civil-Contract Reading of Marriage Authority").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '763fd3a1-4a27-43e5-8c4d-67b19224d647').
narrative_ontology:cs_kernel_codification('763fd3a1-4a27-43e5-8c4d-67b19224d647', formalized).
narrative_ontology:cs_authority_grounding('763fd3a1-4a27-43e5-8c4d-67b19224d647', distributed).
narrative_ontology:cs_reading_relation('763fd3a1-4a27-43e5-8c4d-67b19224d647', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('763fd3a1-4a27-43e5-8c4d-67b19224d647', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('763fd3a1-4a27-43e5-8c4d-67b19224d647', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('763fd3a1-4a27-43e5-8c4d-67b19224d647', family_law_authority__parsi_zoroastrian_reading, influences).
narrative_ontology:cs_axiom('763fd3a1-4a27-43e5-8c4d-67b19224d647', foundational, marital_validity_derives_from_state_registration_alone).
narrative_ontology:cs_axiom_status(marital_validity_derives_from_state_registration_alone, holdable).
narrative_ontology:cs_axiom_grounding('763fd3a1-4a27-43e5-8c4d-67b19224d647', marital_validity_derives_from_state_registration_alone, conventional).
narrative_ontology:cs_axiom('763fd3a1-4a27-43e5-8c4d-67b19224d647', foundational, gender_symmetric_default_rights_regardless_of_religion).
narrative_ontology:cs_axiom_status(gender_symmetric_default_rights_regardless_of_religion, holdable).
narrative_ontology:cs_axiom_grounding('763fd3a1-4a27-43e5-8c4d-67b19224d647', gender_symmetric_default_rights_regardless_of_religion, deontological).
narrative_ontology:cs_reference_frame('763fd3a1-4a27-43e5-8c4d-67b19224d647', state_civil_authority_over_marital_status).
narrative_ontology:cs_drift_state('763fd3a1-4a27-43e5-8c4d-67b19224d647', contemporary_pluralist_democracies, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('763fd3a1-4a27-43e5-8c4d-67b19224d647', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, civil_registry_state).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, women_seeking_gender_symmetric_rights).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, lgbtq_couples_where_recognized).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, religious_authorities_losing_jurisdiction).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, community_elders_enforcing_endogamy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers marriage as a registration act: two competent adults sign, the state records it, and the resulting bundle of rights (inheritance, custody defaults, dissolution procedure) attaches uniformly regardless of the parties' religion. The state's authority to validate marriage does not derive from or require any religious sanction.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_registry_state, agenda_setter,
    institutional, generational, analytical, national).

% Could not marry under any single religious personal-law regime without one party converting or being excluded. Under this reading they register civilly and their marriage is fully valid without reference to either family's religious law. Exit from religious jurisdiction is the entire point for this group.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, mobile, national).

% Under several personal-law regimes, women hold asymmetric rights to initiate divorce, inherit, or retain custody. Under the civil-contract reading these rights are formally symmetric by statute. Practical exit still depends on economic independence and social support, which the reading does not itself supply.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, women_seeking_gender_symmetric_rights, beneficiary,
    moderate, biographical, constrained, national).

% In jurisdictions where the secular reading has been extended to same-sex couples, civil registration is the only avenue to marriage recognition at all, since no religious personal-law regime in this kernel contest offers one. Where the secular reading is not so extended, this group remains excluded from the kernel entirely.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, lgbtq_couples_where_recognized, beneficiary,
    powerless, biographical, constrained, national).

% Ecclesiastical courts, qazis, and community religious bodies that formerly held sole or primary authority over marriage validity within their communities see that authority displaced to a civil registrar for any couple who chooses the civil route. They retain religious/ceremonial authority but lose the legal monopoly on validity, and with it, disciplinary leverage over members contemplating exit from communal marriage norms.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_authorities_losing_jurisdiction, payer,
    organized, generational, constrained, national).

% Rely on the threat of non-recognition or social exile to enforce marriage within caste, sect, or community lines. Civil registration gives couples a legally valid exit from that enforcement — the elders' sanction no longer determines whether the marriage exists in the eyes of the state.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, community_elders_enforcing_endogamy, payer,
    organized, biographical, constrained, local).

% Adjudicate boundary disputes between the civil-contract reading and the personal-law regimes it sits alongside — for example whether a civil marriage can be dissolved under a different personal law than it was contracted under, or whether the state can compel religious bodies to recognize civil marriages for ancillary purposes.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, judiciary_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, religion-neutral procedure by which any two competent adults can obtain state-recognized marital status, portable across the jurisdiction and independent of which (if any) religious community either party belongs to.
% TRANSFER_FUNCTION: Moves the authority to certify marital validity from religious/communal bodies to the state registrar; moves associated dispute-resolution jurisdiction (dissolution, maintenance, succession defaults) from personal-law forums to civil courts for couples who register this way.
% ABSENT_VOICES: Religious authorities and community elders are not formally excluded from public debate but are excluded from the transaction itself once a couple elects the civil route — their consent is not required and their objection has no legal effect on validity. Conservative co-religionists who view civil marriage as a threat to communal cohesion are rarely heard in the drafting of civil marriage statutes.
% DISAPPEARANCE_RATIONALE: If the civil-contract option vanished, interfaith and interfaith-adjacent couples would have no legal path to marriage without conversion or exclusion; individuals with no religious affiliation would have no route to state-recognized marriage at all; and jurisdictions that extended the secular reading to same-sex couples would lose that recognition entirely. Personal-law regimes would revert to full monopoly over marital status.
% FOUNDING_PROBLEM: Religiously plural and increasingly secularizing states needed a marriage procedure that did not require citizens to profess or perform a particular religion's rites, and that could resolve interfaith unions and religiously unaffiliated citizens' marriages without forcing conversion.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts in multiple pluralist states (attesting from outside both the civil registry and the religious authorities) have repeatedly affirmed continuing demand for civil marriage as the only viable route for interfaith and non-religious couples; census and registry data independent of state marketing show sustained uptake rather than a legacy option nobody uses.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.28) and has drifted slightly downward then stabilized: the arrangement's coordination function (a religion-neutral validity procedure) is genuine and does not extract rents from participants who choose it. Suppression is low-moderate (0.22): the state does not compel anyone into civil marriage, but does compel recognition of its legal effects once chosen, and in some jurisdictions restricts personal-law dissolution recognition for civilly-contracted marriages. Theater ratio is low and only mildly rising (0.15 by 2025), reflecting some accumulation of procedural formalism (waiting periods, notice requirements) without displacing the core function. Accessibility collapse is moderate (0.35) — for couples within a single religious tradition who are content with personal law, the civil alternative is a genuine option, not a forced substitute, so alternatives have not collapsed; the state has not eliminated personal-law marriage, it has added a parallel track.
 *
 * DIRECTIONALITY LOGIC:
 *   Interfaith couples, women seeking symmetric default rights, and (where extended) LGBTQ couples sit near the beneficiary end: the arrangement subsidizes access that would otherwise be denied or made conditional on conversion. Religious authorities and community elders sit toward the target end: the arrangement extracts jurisdiction and enforcement leverage from them without their consent, though the extraction is jurisdictional rather than material. The civil registry state itself is the agenda-setter with no directionality cost — it is the administering seat, not a payer or beneficiary in the distributive sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling marriage across religious lines and for the non-religious — remains live rather than obsolete; demand for civil marriage as a distinct option has not declined as pluralism has increased, if anything the reverse. This blocks a mandatrophy read: this is not a coordination function persisting past its use, it is a coordination function whose use case has expanded (extension to same-sex couples in some jurisdictions is evidence of a growing rather than shrinking constituency).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civil_registration_universality_vs_supplementary_track,
    'Does the secular contractual reading function as the universal default validity test (displacing personal law entirely) or as a supplementary, opt-in track that coexists with personal-law regimes for those who prefer it?',
    'Examine specific jurisdictional statutes: does civil registration preempt personal-law marriage validity for all citizens, or only provide an alternative route available alongside continuing personal-law jurisdiction for those who do not opt in?',
    'If universal/preemptive, extraction from religious authorities is total and the reading functions closer to a snare against those authorities; if supplementary, the low extractiveness and rope classification are well-supported since personal-law communities retain their own track undisturbed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_registration_universality_vs_supplementary_track, empirical, 'Whether this reading preempts or merely supplements personal-law marriage regimes.').

omega_variable(
    formal_vs_substantive_gender_symmetry,
    'Does gender-symmetric statutory language translate into substantive parity in outcomes (divorce initiation, custody, property division) or does social and economic asymmetry persist despite formal symmetry?',
    'Compare divorce/custody/maintenance outcome data for civilly-married couples against the statutory symmetry claim; track whether courts apply the symmetric text evenly across genders in practice.',
    'If substantive asymmetry persists, the beneficiary classification for women is overstated and the reading''s coordination benefit for that group is partly theatrical rather than real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_substantive_gender_symmetry, empirical, 'Gap between statutory gender symmetry and practical outcome parity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__secular_contractual_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(fami_tr_t1965, family_law_authority__secular_contractual_reading, theater_ratio, 1965, 0.11).
narrative_ontology:measurement(fami_tr_t1980, family_law_authority__secular_contractual_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(fami_tr_t1995, family_law_authority__secular_contractual_reading, theater_ratio, 1995, 0.13).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__secular_contractual_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(fami_tr_t2025, family_law_authority__secular_contractual_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(fami_be_t1950, family_law_authority__secular_contractual_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(fami_be_t1965, family_law_authority__secular_contractual_reading, base_extractiveness, 1965, 0.3).
narrative_ontology:measurement(fami_be_t1980, family_law_authority__secular_contractual_reading, base_extractiveness, 1980, 0.29).
narrative_ontology:measurement(fami_be_t1995, family_law_authority__secular_contractual_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__secular_contractual_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(fami_be_t2025, family_law_authority__secular_contractual_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(family_law_authority__secular_contractual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

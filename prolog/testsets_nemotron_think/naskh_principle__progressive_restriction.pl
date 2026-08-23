% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction Reading of Naskh (Quranic Hermeneutics)
 *   domain: religious/legal/hermeneutics
 *
 * SUMMARY:
 *   The progressive restriction reading of naskh (Quranic abrogation) argues
 *   that revelation moved from permissive to restrictive rulings as divine
 *   pedagogy (tadarruj/tathir) — accommodating human capacity at each stage —
 *   rather than invalidating earlier verses. Earlier permissions (e.g.,
 *   gradual prohibition of alcohol, regulated slavery, defensive warfare
 *   rules) are understood as transitional accommodations (rukhsah) superseded
 *   by final restrictive mandates (azimah) that represent the complete divine
 *   intent. This reading avoids claiming God 'changed His mind' (bada') while
 *   allowing legal evolution. It benefits reformist scholars seeking Quranic
 *   authority for modern restrictions (e.g., total alcohol ban, slavery
 *   abolition, gender equality) but extracts interpretive authority from
 *   classical scholars whose methodology centers naskh as chronological
 *   supersession. Victims include traditionalists who cite earlier permissive
 *   verses for contemporary practice and communities whose practices are
 *   delegitimized by the 'transitional' designation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.42).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.38).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.42).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction Reading of Naskh (Quranic Hermeneutics)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '706647ed-78a9-4e75-a993-780bea5cbce6').
narrative_ontology:cs_kernel_codification('706647ed-78a9-4e75-a993-780bea5cbce6', formalized).
narrative_ontology:cs_authority_grounding('706647ed-78a9-4e75-a993-780bea5cbce6', lineage).
narrative_ontology:cs_interpretation_layer_present('706647ed-78a9-4e75-a993-780bea5cbce6').
narrative_ontology:cs_reading_relation('706647ed-78a9-4e75-a993-780bea5cbce6', naskh_principle__classical_abrogation, forecloses).
narrative_ontology:cs_reading_relation('706647ed-78a9-4e75-a993-780bea5cbce6', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('706647ed-78a9-4e75-a993-780bea5cbce6', foundational, divine_pedagogy_not_abrogation).
narrative_ontology:cs_axiom_status(divine_pedagogy_not_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('706647ed-78a9-4e75-a993-780bea5cbce6', divine_pedagogy_not_abrogation, deontological).
narrative_ontology:cs_axiom('706647ed-78a9-4e75-a993-780bea5cbce6', foundational, earlier_permissions_transitional).
narrative_ontology:cs_axiom_status(earlier_permissions_transitional, holdable).
narrative_ontology:cs_axiom_grounding('706647ed-78a9-4e75-a993-780bea5cbce6', earlier_permissions_transitional, empirically_contingent).
narrative_ontology:cs_axiom('706647ed-78a9-4e75-a993-780bea5cbce6', secondary, final_restrictive_intent_teleology).
narrative_ontology:cs_axiom_status(final_restrictive_intent_teleology, holdable).
narrative_ontology:cs_axiom_grounding('706647ed-78a9-4e75-a993-780bea5cbce6', final_restrictive_intent_teleology, deontological).
narrative_ontology:cs_reference_frame('706647ed-78a9-4e75-a993-780bea5cbce6', classical_naskh_doctrine).
narrative_ontology:cs_drift_state('706647ed-78a9-4e75-a993-780bea5cbce6', modern_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('706647ed-78a9-4e75-a993-780bea5cbce6', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, progressive_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, reformist_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, modernist_interpreters).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, classical_abrogation_adherents).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_traditionalists).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, permissive_verse_practitioners).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_pedagogy_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, revelation_as_gradual_guidance).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, legal_evolution_within_fixed_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and propagate the progressive restriction methodology through academic institutions, fatwa councils, and public discourse. They gain interpretive authority and relevance in modern contexts by offering a framework that preserves Quranic authority while allowing legal evolution. Their exit options include moving to secular legal academia or other interpretive traditions.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, progressive_scholars, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, progressive_scholars, beneficiary).

% Apply progressive restriction in practical legal reform (family law, finance, criminal penalties). They benefit from methodological cover for reforms but face institutional pushback from classical establishments. Exit means either conforming to classical methodology or leaving official juristic roles.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, reformist_jurists, beneficiary,
    organized, biographical, constrained, national).

% Use the reading to articulate Islam-compatible modernity in theology, ethics, and politics. They gain intellectual credibility but remain marginal in traditional seminaries. Exit options include secular frameworks or more radical reform movements.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, modernist_interpreters, beneficiary,
    moderate, biographical, constrained, global).

% Hold the classical naskh doctrine as essential to Quranic coherence and scholarly authority. They lose hermeneutic ground when progressive restriction gains traction, as it denies the abrogation mechanism they consider divinely established. Their identity is fused to the classical madhhab system; exit means abandoning their scholarly lineage and communal authority.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_adherents, payer,
    institutional, civilizational, identity_locked, global).

% Read Quranic verses as permanently valid literal commands. Progressive restriction undermines their claim that earlier permissive verses (e.g., on slavery, warfare, gender) remain operative. They lack institutional power to suppress the reading but resist through grassroots authority. Exit is nearly impossible without theological rupture.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, literalist_traditionalists, payer,
    organized, generational, trapped, regional).

% Communities and individuals who rely on earlier permissive verses for current practice (e.g., certain marital rights, dietary allowances, warfare rules). When progressive restriction declares these transitional, their practices are delegitimized without clear replacement. They have limited scholarly recourse and face social pressure to conform.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, permissive_verse_practitioners, payer,
    powerless, immediate, constrained, local).

% Advocate for context-specific validity of all verses without chronological supersession. They are excluded from the progressive restriction vs. classical abrogation binary despite offering a third coherent methodology. Their voice would complicate the debate but is rarely centered in institutional debates.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_adherents, excluded,
    moderate, biographical, mobile, global).

% Scholars of Islamic studies, comparative law, and hermeneutics who analyze the debate from outside the tradition's authority structures. They document the methodological stakes but do not participate in the internal contest over legitimacy.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, academic_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutic that reconciles apparently contradictory Quranic rulings without invoking divine abrogation (naskh), preserving the text's perpetual validity while allowing legal rulings to evolve toward a restrictive telos understood as final divine intent.
% TRANSFER_FUNCTION: Moves interpretive authority from the chronological-abrogation mechanism (which declares earlier verses mansukh/abrogated) to a teleological-pedagogical mechanism (which declares earlier verses transitional accommodations). The transfer is from classical madhhab authority to modernist/reformist scholarly networks.
% ABSENT_VOICES: Classical abrogation proponents (majority of traditional ulama) who view progressive restriction as denying an explicit Quranic mechanism (2:106, 16:101) and undermining the isnad-based authority chain. Contextual harmonization proponents (e.g., Fazlur Rahman's 'double movement', Abu Zayd's semantic historicism) who reject both chronological supersession and fixed teleology but are marginalized in institutional fatwa bodies. Pre-modern minority voices (some Zahiri, early Mutazila) who anticipated elements of this reading but were historically suppressed.
% DISAPPEARANCE_RATIONALE: If progressive restriction vanished, the primary hermeneutic bridge between fixed Quranic text and evolutionary legal reform in modern nation-states would collapse. Reformist jurists would lose methodological cover for reforms in family law, penal law, and finance. Classical abrogation would regain unchallenged dominance in traditional institutions. The global Muslim reform discourse would lose its most textually grounded framework.
% FOUNDING_PROBLEM: How to maintain the Quran's status as uncreated, eternal, and non-contradictory divine speech while accounting for clear historical progression in Quranic legislation (e.g., alcohol, slavery, warfare, inheritance, gender rules) that appears to restrict earlier permissions.
% FOUNDING_PROBLEM_CORROBORATION: Modernist scholars (Fazlur Rahman, Abdullahi An-Na'im, Khaled Abou El Fadl) attest the problem remains live and the reading addresses it. Classical scholars (Al-Azhar establishment, Deobandi ulama, Salafi institutions) attest the problem was solved by naskh doctrine and progressive restriction is a modern innovation. Western Islamicists (Wansbrough, Crone, later scholars) corroborate the historical reality of legislative progression in the text but contest the theological resolution. No consensus exists across the epistemic communities.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).
:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the reading extracts interpretive capital from classical authorities and restricts the operative verse-set for practitioners of earlier permissions. Suppression (0.38) is moderate: it requires active scholarly enforcement (fatwa councils, curriculum control, institutional appointments) to marginalize classical naskh, but does not physically coerce. Theater ratio (0.18) is low: the pedagogical narrative performs genuine hermeneutic work, not mere ritual. Accessibility collapse (0.55) is moderate: classical naskh and contextual harmonization remain live alternatives but are institutionally disadvantaged. Resistance (0.62) is high: the reading faces sustained opposition from the majority of traditional institutions globally.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive scholar seat, the constraint is a rope: genuine coordination solving the contradiction problem with minimal coercion. From the classical adherent seat, it is a snare: extraction of their hermeneutic capital under cover of 'pedagogy.' From the permissive practitioner seat, it is a tangled rope: they lose concrete permissions (extraction) but gain a framework that could theoretically protect them from harsher restrictions (coordination). The engine computes this divergence from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars and reformist jurists are structural beneficiaries (d ~ 0.2): they gain interpretive authority, reform legitimacy, and modern relevance. Classical abrogation adherents are primary targets (d ~ 0.85): their core methodology is denied, their authority eroded, and their identity is fused to the classical system (identity_locked). Literalist traditionalists are trapped targets (d ~ 0.9): they lack institutional power to resist and face delegitimization of their practices. Permissive verse practitioners are constrained payers (d ~ 0.7): they bear the cost of lost permissions with limited recourse. Contextual harmonization adherents are excluded (mobile, d ~ 0.4): they occupy a third position but are structurally crowded out of the binary debate.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (reconciling textual fixity with legislative progression) remains live — the Quran's internal progression is a permanent hermeneutic fact. No mandatrophy: the constraint continues to solve its founding problem for its beneficiaries. However, classical naskh doctrine shows mandatrophy signs: its founding problem (managing contradiction in a fixed canon) was solved by a mechanism (chronological abrogation) that now obstructs the very fixity it protected, as modern contexts demand restrictions (e.g., on slavery) that classical naskh cannot easily produce without declaring verses abrogated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogy_vs_abrogation_ontology,
    'Is ''divine pedagogy'' (tadarruj) a genuine ontological category in the Quranic worldview, or a modern hermeneutic projection onto the text?',
    'Comparative analysis of pre-modern tafsir: do classical scholars (Tabari, Qurtubi, Razi, Ibn Kathir) ever describe naskh as pedagogy rather than abrogation? If the category is absent pre-modernly, it is a projection.',
    'If projection, the reading''s claimed_type (rope) is undermined — it becomes a tangled rope extracting classical authority under false pretenses. If genuine, the reading accesses an internal Islamic hermeneutic resource, strengthening its coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_vs_abrogation_ontology, conceptual, 'Whether the core axiom ''divine pedagogy not abrogation'' is internally grounded or externally imposed.').

omega_variable(
    transitional_designation_criteria,
    'What are the objective criteria for designating a verse as ''transitional accommodation'' vs. ''permanent mandate'' within this reading?',
    'Survey progressive scholars'' methodological writings (Rahman, An-Na''im, Abu Zayd, Soroush, etc.) for explicit criteria. Test inter-rater reliability: do independent scholars classify the same verses the same way?',
    'If criteria are ad hoc or outcome-driven, the reading''s extractiveness is higher than measured — it functions as a results-oriented tool. If stable criteria exist, the reading has genuine methodological coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transitional_designation_criteria, empirical, 'Whether the reading has a stable methodological core or operates as post-hoc justification.').

omega_variable(
    permissive_verse_practitioner_agency,
    'Do communities practicing earlier permissions (e.g., gradualist alcohol approaches, regulated concubinage in historical contexts) experience the ''transitional'' designation as coercive suppression or as liberated reinterpretation?',
    'Ethnographic study of minority communities (e.g., some Sufi orders, historical communities) whose practices align with ''abrogated'' permissions. Document their self-understanding under progressive restriction discourse.',
    'If experienced as coercive, suppression is under-measured and the reading trends toward snare for powerless payers. If experienced as liberation, the reading''s coordination function extends to these payers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permissive_verse_practitioner_agency, empirical, 'The lived experience of the most vulnerable payer group under this reading.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the naskh_principle kernel admit a single coherent framing, or do the three readings (classical_abrogation, contextual_harmonization, progressive_restriction) operate on fundamentally different conceptions of what ''naskh'' names?',
    'Analyze whether the sibling readings share a common object of disagreement (the mechanism of verse-relations) or talk past each other (one names a chronological mechanism, one names a contextual validity condition, one names a teleological trajectory).',
    'If framings are incommensurable, the kernel is distributed/implicit rather than formalized, and the cs_structure authority_grounding may need revision. The reading_relations would shift from forecloses/coexists to a more complex structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel contest is a genuine three-way dispute over one object or a category error.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 1850, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_prog_restrict_tr_t1850, naskh_principle__progressive_restriction, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(naskh_prog_restrict_tr_t1900, naskh_principle__progressive_restriction, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(naskh_prog_restrict_tr_t1950, naskh_principle__progressive_restriction, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(naskh_prog_restrict_tr_t1975, naskh_principle__progressive_restriction, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(naskh_prog_restrict_tr_t2000, naskh_principle__progressive_restriction, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(naskh_prog_restrict_tr_t2025, naskh_principle__progressive_restriction, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(naskh_prog_restrict_be_t1850, naskh_principle__progressive_restriction, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(naskh_prog_restrict_be_t1900, naskh_principle__progressive_restriction, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(naskh_prog_restrict_be_t1950, naskh_principle__progressive_restriction, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(naskh_prog_restrict_be_t1975, naskh_principle__progressive_restriction, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(naskh_prog_restrict_be_t2000, naskh_principle__progressive_restriction, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(naskh_prog_restrict_be_t2025, naskh_principle__progressive_restriction, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(naskh_prog_restrict_su_t1850, naskh_principle__progressive_restriction, suppression_requirement, 1850, 0.1).
narrative_ontology:measurement(naskh_prog_restrict_su_t1900, naskh_principle__progressive_restriction, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(naskh_prog_restrict_su_t1950, naskh_principle__progressive_restriction, suppression_requirement, 1950, 0.32).
narrative_ontology:measurement(naskh_prog_restrict_su_t1975, naskh_principle__progressive_restriction, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(naskh_prog_restrict_su_t2000, naskh_principle__progressive_restriction, suppression_requirement, 2000, 0.37).
narrative_ontology:measurement(naskh_prog_restrict_su_t2025, naskh_principle__progressive_restriction, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__progressive_restriction, 0.08).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, quranic_hermeneutic_authority).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, islamic_legal_reform_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is one member of the naskh_principle constraint family. The kernel 'naskh_principle' decomposes into three structurally distinct readings with different ε values: classical_abrogation (low ε, mountain-like for adherents), contextual_harmonization (moderate ε, rope-like), progressive_restriction (moderate-high ε, rope/tangled_rope). They are linked because classical_abrogation is historically upstream and its authority is cited by the other two as the foil; progressive_restriction and contextual_harmonization both position themselves against classical_abrogation but differ on whether teleology or context is the alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__progressive_restriction, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

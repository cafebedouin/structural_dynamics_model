% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Domain-Bifurcated Latin Correctness (Classical for Letters, Medieval for Craft)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This story instantiates the 'hybrid reading' of the contested Latin
 *   correctness kernel: it holds that classical Ciceronian norms are the
 *   legitimate standard for literary and rhetorical composition, while
 *   medieval Latin forms remain fully legitimate — not corrupted, not merely
 *   tolerated — for technical, legal, and administrative writing. This is a
 *   real settlement humanist pedagogy and print culture converged on across
 *   the 14th-16th centuries, distinct from both the pure continuity position
 *   (medieval Latin is simply Latin's organic continuation everywhere) and
 *   the pure rupture position (classical Latin is the only legitimate
 *   standard and medieval usage is corruption requiring correction
 *   everywhere). The hybrid reading's distinguishing structural feature is
 *   the domain boundary itself: it creates a status hierarchy where literary
 *   achievement outranks technical competence, and it generates a partial
 *   victim class — technical writers who are formally exempted from classical
 *   standards but informally pressured toward them by gatekeepers whose
 *   prestige economy runs on classical register.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.52).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.48).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Domain-Bifurcated Latin Correctness (Classical for Letters, Medieval for Craft)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '80aa3f18-3ebd-4c44-a509-4323504f1633').
narrative_ontology:cs_kernel_codification('80aa3f18-3ebd-4c44-a509-4323504f1633', distributed).
narrative_ontology:cs_authority_grounding('80aa3f18-3ebd-4c44-a509-4323504f1633', practice).
narrative_ontology:cs_interpretation_layer_present('80aa3f18-3ebd-4c44-a509-4323504f1633').
narrative_ontology:cs_reading_relation('80aa3f18-3ebd-4c44-a509-4323504f1633', latin_correctness__continuity_reading, influences).
narrative_ontology:cs_reading_relation('80aa3f18-3ebd-4c44-a509-4323504f1633', latin_correctness__rupture_reading, influences).
narrative_ontology:cs_axiom('80aa3f18-3ebd-4c44-a509-4323504f1633', foundational, domain_determines_register_legitimacy).
narrative_ontology:cs_axiom_status(domain_determines_register_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('80aa3f18-3ebd-4c44-a509-4323504f1633', domain_determines_register_legitimacy, conventional).
narrative_ontology:cs_axiom('80aa3f18-3ebd-4c44-a509-4323504f1633', secondary, literary_register_ranks_above_technical_register).
narrative_ontology:cs_axiom_status(literary_register_ranks_above_technical_register, holdable).
narrative_ontology:cs_axiom_grounding('80aa3f18-3ebd-4c44-a509-4323504f1633', literary_register_ranks_above_technical_register, conventional).
narrative_ontology:cs_reference_frame('80aa3f18-3ebd-4c44-a509-4323504f1633', ciceronian_literary_medieval_technical_split).
narrative_ontology:cs_drift_state('80aa3f18-3ebd-4c44-a509-4323504f1633', print_era_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80aa3f18-3ebd-4c44-a509-4323504f1633', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_literary_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_rhetoric_educators).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_and_scientific_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, vernacular_adjacent_notaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, print_and_university_gatekeepers).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, domain_appropriate_register_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and police the standard that literary and rhetorical Latin must reconstruct Ciceronian usage, teaching this standard in academies and courts. They hold the cultural capital that comes from mastering the harder, restricted register and collect prestige, patronage, and teaching posts from certifying who has achieved it. Their exit from the standard is costless — they invented it and can redefine it.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_literary_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, humanist_literary_scholars, beneficiary).

% Teach the bifurcated standard to elite pupils, charging for access to classical composition training. They benefit from the hierarchy that makes literary Latin scarce and valuable while technical Latin remains 'merely useful' and uncredentialed. They could relocate their teaching practice but have no incentive to dismantle the distinction that funds them.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_rhetoric_educators, beneficiary,
    organized, generational, mobile, regional).

% Write medical, legal, and natural-philosophical treatises in inherited medieval Latin forms that function perfectly for their purposes. Periodically pressured — in patronage applications, in university disputations, in printing-house peer review — to 'elevate' their prose toward classical models they were never trained in, at cost to clarity and to their own time. They cannot simply opt out if they want their work read by the literary-credentialed gatekeepers who sit on funding and licensing bodies.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_and_scientific_writers, payer,
    moderate, biographical, constrained, regional).

% Draft contracts, wills, and administrative records in functional medieval-derived Latin. They have no access to classical training at all, yet the bifurcated standard leaves their register formally 'legitimate but lesser' — permanently excluded from higher status regardless of their documents' functional adequacy. Their exit option is nonexistent: their livelihood depends on the register they already use.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, vernacular_adjacent_notaries, payer,
    powerless, biographical, trapped, local).

% Control which manuscripts receive prestigious publication, which theses are accepted, and which authors are cited as models. They enforce the domain split by rewarding classical register in literary submissions while merely tolerating medieval register in technical submissions, entrenching the hierarchy through selection rather than explicit rule.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, print_and_university_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, print_and_university_gatekeepers, beneficiary).

% Study the historical record of both registers without a stake in either camp's prestige economy. They can see that the domain-bifurcation was itself a contingent settlement of a live dispute between continuity and rupture positions, not a neutral linguistic fact.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, philologists_of_later_centuries, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working division of linguistic labor: classical register signals formal literary/rhetorical achievement while medieval-derived register remains fully functional and unstigmatized for technical, legal, and administrative writing that needs precision and established terminology more than ornamental style.
% TRANSFER_FUNCTION: Moves prestige, patronage, and institutional credentialing toward those who master classical composition, while technical writers who could write perfectly functional medieval-register prose absorb reputational costs and extra unpaid labor when literary-credentialed gatekeepers judge their submissions by classical standards anyway.
% ABSENT_VOICES: Vernacular-adjacent notaries and provincial technical writers have no seat in the academies or printing houses that set the terms of the bifurcation; they would object that the 'legitimate for technical domains' clause is honored unevenly and used selectively to gatekeep advancement even within technical fields.
% DISAPPEARANCE_RATIONALE: Humanist scholars and educators would say the world rearranges catastrophically — literary standards collapse without the classical/medieval domain split. Technical writers and notaries would say little changes for their daily practice, since their register was never dependent on the bifurcation being formally recognized; what would change is the removal of periodic status pressure to imitate an inappropriate register.
% FOUNDING_PROBLEM: Renaissance humanists needed to explain why medieval Latin, which had served European learned communication for eight centuries, should be displaced in literary contexts without simultaneously discrediting the vast technical, legal, and scientific corpus written in that same medieval register that Europe still depended on.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars themselves attest the domain split solved a real coordination problem (preserving classical rhetoric's prestige while not disrupting functioning technical literatures). Independent corroboration is thinner: later philologists and historians of science note that the split's boundary was drawn and redrawn opportunistically by whoever controlled publication gatekeeping in a given period, and that technical writers were never consulted on where the boundary should fall.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, contested).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than high: the domain split genuinely protects most technical writers most of the time — this is not systematic extraction across all Latin use, only at the boundary where gatekeepers apply literary standards to technical submissions inconsistently. Suppression is moderate (0.48): there is no formal prohibition on medieval-register technical writing, but informal reputational and institutional pressure functions as real suppressive force at the boundary. Theater ratio is moderate-high (0.4) and rising: as the split calcifies over centuries, an increasing share of 'upholding standards' activity in the technical domain becomes performative deference to classical models that serve no functional purpose in a legal contract or medical formulary. The rising suppression_requirement trajectory reflects the gradual institutionalization of the boundary through print-house and university selection practices rather than any single decree.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist scholar's seat this looks like sensible domain-appropriate stylistics — a rope. From the notary's seat, permanently excluded from the higher-status register by lack of access rather than by choice, the same structure reads as a caste system dressed as a style guide. The engine computes these divergently from the declared power/exit data; the claimed_type of tangled_rope is the story-level judgment that both readings are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars and classical rhetoric educators are the structural beneficiaries: the domain split is precisely the settlement that lets them monopolize prestige without having to discredit the technical corpus outright (which would provoke resistance they can't win) or cede any literary ground (which would cost their status). Technical and scientific writers are partial victims: protected in principle by the hybrid settlement, they nonetheless pay whenever gatekeepers apply literary standards inconsistently at review and publication. Vernacular-adjacent notaries are the deepest victims: they have no path to the classical register at all and are permanently fixed at the bottom of the hierarchy the hybrid reading creates, with no exit since their register is inseparable from their livelihood.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists collapsing into either 'pure snare' (rupture reading, which would discredit all medieval Latin) or 'pure rope' (continuity reading, which claims no hierarchy exists). Classifying it as tangled_rope captures both halves honestly: it does solve a genuine problem (avoiding wholesale disruption of a working technical literature) AND it does extract status and resources asymmetrically toward those who master the harder, rarer register. Neither pure-coordination nor pure-extraction framing would be accurate; the hybrid reading is exactly the coordination-with-a-toll structure tangled_rope names.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_enforcement_consistency,
    'Was the classical/medieval domain boundary applied consistently by gatekeepers, or was it selectively invoked to exclude specific authors or subject areas regardless of formal domain classification?',
    'Archival analysis of print-house rejection letters, university disputation records, and patronage correspondence across the 14th-16th centuries to determine whether medical or legal writers received classical-standard criticism at rates disproportionate to literary writers receiving medieval-standard tolerance.',
    'If enforcement was consistent with the stated domain boundary, extractiveness should be revised downward toward genuine rope; if enforcement was selectively deployed against specific writers or subjects, extractiveness should be revised upward and the tangled_rope classification strengthened toward snare for the affected subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_enforcement_consistency, empirical, 'Whether the domain boundary was applied evenly or used as a discretionary gatekeeping tool.').

omega_variable(
    hybrid_reading_stability,
    'Is the domain-bifurcation itself a stable, defensible linguistic-sociological category, or is it a post-hoc rationalization that different parties draw the boundary differently depending on their interests (as the continuity_reading and rupture_reading camps each claim)?',
    'Compare where different historical actors (humanist pedagogues vs. print censors vs. professional guilds) drew the literary/technical boundary for the same genres (e.g., scientific poetry, legal rhetoric) to test whether the boundary tracks a real functional distinction or is drawn to serve whichever party is drawing it.',
    'If the boundary is stable across independent framers, the hybrid reading''s coordination function is more genuine than its extraction; if the boundary shifts opportunistically by framer, the coordination story is closer to cover for status extraction, supporting reclassification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_stability, conceptual, 'Whether the hybrid reading''s domain boundary is a real category or a discretionary rationalization — this is the specific structural element on which this reading and its two siblings disagree.').

omega_variable(
    sibling_reading_resource_competition,
    'Does the hybrid reading''s dominance in institutional practice suppress resources (teaching posts, print runs, patronage) available to advocates of the pure continuity_reading or pure rupture_reading positions?',
    'Trace institutional curriculum records to see whether continuity-position and rupture-position pedagogues lost posts or funding specifically because the hybrid settlement became the institutionally dominant compromise.',
    'If hybrid dominance measurably starved the sibling readings of institutional resources, this constraint''s influences edge toward both siblings should be treated as a stronger causal claim rather than mere coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_resource_competition, empirical, 'Whether the hybrid reading''s institutional dominance materially disadvantaged the two sibling readings'' proponents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lati_tr_t50, latin_correctness__hybrid_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(lati_tr_t100, latin_correctness__hybrid_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(lati_tr_t150, latin_correctness__hybrid_reading, theater_ratio, 150, 0.34).
narrative_ontology:measurement(lati_tr_t200, latin_correctness__hybrid_reading, theater_ratio, 200, 0.37).
narrative_ontology:measurement(lati_tr_t250, latin_correctness__hybrid_reading, theater_ratio, 250, 0.39).
narrative_ontology:measurement(lati_tr_t300, latin_correctness__hybrid_reading, theater_ratio, 300, 0.4).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lati_be_t50, latin_correctness__hybrid_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(lati_be_t100, latin_correctness__hybrid_reading, base_extractiveness, 100, 0.46).
narrative_ontology:measurement(lati_be_t150, latin_correctness__hybrid_reading, base_extractiveness, 150, 0.5).
narrative_ontology:measurement(lati_be_t200, latin_correctness__hybrid_reading, base_extractiveness, 200, 0.51).
narrative_ontology:measurement(lati_be_t250, latin_correctness__hybrid_reading, base_extractiveness, 250, 0.52).
narrative_ontology:measurement(lati_be_t300, latin_correctness__hybrid_reading, base_extractiveness, 300, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lati_su_t50, latin_correctness__hybrid_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(lati_su_t100, latin_correctness__hybrid_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(lati_su_t150, latin_correctness__hybrid_reading, suppression_requirement, 150, 0.44).
narrative_ontology:measurement(lati_su_t200, latin_correctness__hybrid_reading, suppression_requirement, 200, 0.46).
narrative_ontology:measurement(lati_su_t250, latin_correctness__hybrid_reading, suppression_requirement, 250, 0.47).
narrative_ontology:measurement(lati_su_t300, latin_correctness__hybrid_reading, suppression_requirement, 300, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the Latin correctness debate' per the ε-invariance principle: continuity_reading (no domain split, medieval Latin fully legitimate everywhere), hybrid_reading (this story — classical for literary, medieval for technical), and rupture_reading (classical only, medieval is corruption everywhere). Each carries its own ε, beneficiary/victim structure, and claimed_type. The hybrid reading sits structurally between the other two on extractiveness precisely because it partially concedes the continuity position (technical domain) while partially adopting the rupture position (literary domain) — this is authored as a fact about this reading's own structure, not as an average of the siblings' ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

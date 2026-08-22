% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Homoiousian (Similar-Substance) Christological Compromise
 *   domain: religious/political
 *
 * SUMMARY:
 *   In the decades following Nicaea (325), the eastern church fractured into
 *   factions unable to agree whether Christ's substance is identical to
 *   (homoousios), similar to (homoiousios), or unlike (anomoios/heterousios)
 *   the Father's. The homoiousian formula, championed by Basil of Ancyra and
 *   a bloc of eastern moderate bishops and backed intermittently by Emperor
 *   Constantius II, functioned as a coordination device: a formula broad
 *   enough that neither strict Nicenes nor strict Arians could be fully
 *   satisfied, but narrow enough to exclude the most extreme Anomoean
 *   position. It held councils and communion together through the 350s but
 *   was progressively absorbed into the Cappadocian-brokered pro-Nicene
 *   settlement ratified at Constantinople in 381. This story authors the
 *   homoiousian reading only — the Arian and pro-Nicene readings of the same
 *   underlying kernel (the substance-relation of Christ to the Father) are
 *   separate constraints with their own ε and structural data, linked here
 *   via network and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - eastern_episcopal_moderates: Primary agenda_setter (institutional/constrained) — drafts and defends the compromise formula
 *   - constantius_imperial_court: Primary beneficiary (institutional/arbitrage) — backs the formula for imperial ecclesiastical peace
 *   - strict_nicene_theologians: Primary target (moderate/trapped) — exiled and excluded under homoiousian-favoring regimes
 *   - strict_arian_clergy: Secondary target (moderate/constrained) — condemned by the same councils that reject Nicene language
 *   - church_historians: Analytical observer — reconstructs the councils and formula sequence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.42).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, scaffold).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousian (Similar-Substance) Christological Compromise").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "religious/political").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).
narrative_ontology:has_sunset_clause(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, 'f15addb7-5dc0-4d13-98bf-1a41914911dc').
narrative_ontology:cs_kernel_codification('f15addb7-5dc0-4d13-98bf-1a41914911dc', distributed).
narrative_ontology:cs_authority_grounding('f15addb7-5dc0-4d13-98bf-1a41914911dc', distributed).
narrative_ontology:cs_reading_relation('f15addb7-5dc0-4d13-98bf-1a41914911dc', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('f15addb7-5dc0-4d13-98bf-1a41914911dc', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('f15addb7-5dc0-4d13-98bf-1a41914911dc', foundational, substance_similarity_suffices_for_communion).
narrative_ontology:cs_axiom_status(substance_similarity_suffices_for_communion, overridden).
narrative_ontology:cs_axiom_grounding('f15addb7-5dc0-4d13-98bf-1a41914911dc', substance_similarity_suffices_for_communion, theological).
narrative_ontology:cs_axiom('f15addb7-5dc0-4d13-98bf-1a41914911dc', secondary, doctrinal_precision_may_be_deferred_for_ecclesial_unity).
narrative_ontology:cs_axiom_status(doctrinal_precision_may_be_deferred_for_ecclesial_unity, holdable).
narrative_ontology:cs_axiom_grounding('f15addb7-5dc0-4d13-98bf-1a41914911dc', doctrinal_precision_may_be_deferred_for_ecclesial_unity, instrumental).
narrative_ontology:cs_reference_frame('f15addb7-5dc0-4d13-98bf-1a41914911dc', post_nicene_unresolved_consensus).
narrative_ontology:cs_drift_state('f15addb7-5dc0-4d13-98bf-1a41914911dc', council_of_constantinople_381, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('f15addb7-5dc0-4d13-98bf-1a41914911dc', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, eastern_episcopal_moderates).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, constantius_imperial_court).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, unity_seeking_congregations).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, strict_nicene_theologians).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, strict_arian_clergy).
narrative_ontology:constraint_vindicates(homoousios_christology__semi_arian_reading, conciliar_compromise_can_preserve_communion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops centered in Asia Minor and Syria (Basil of Ancyra and allies) who draft and promote the homoiousios formula at the councils of the 350s as a via media that avoids both what they see as Sabellian-tinged homoousios language and outright Arian subordinationism. They administer the compromise through regional synods and depend on continued imperial favor to keep it the operative standard in the East.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, eastern_episcopal_moderates, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, eastern_episcopal_moderates, beneficiary).

% Emperor Constantius II backs the homoiousian settlement (Councils of Sirmium, Seleucia, Constantinople 359-360) because a broad doctrinal middle preserves ecclesiastical peace and imperial control over church appointments across a fractious empire. He can shift support to whichever formula best serves unity and does not personally bear doctrinal risk.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, constantius_imperial_court, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, constantius_imperial_court, agenda_setter).

% Ordinary clergy and laity in the eastern provinces who experience repeated depositions, exiles, and rival bishops installed over the same sees. The homoiousian compromise offers them a formula their local bishops can preach without immediate charges of heresy from either extreme, reducing (for a time) the disruption of factional purges in their own congregations.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, unity_seeking_congregations, beneficiary,
    powerless, biographical, trapped, regional).

% Athanasius and allied Western bishops regard homoiousios as a distinction without theological substance that leaves the door open to subordinating the Son. They are exiled, deposed, or excluded from communion under homoiousian-favoring emperors and synods; their only real recourse is appeal to Rome and endurance until political winds shift.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, strict_nicene_theologians, payer,
    moderate, civilizational, trapped, continental).

% Anomoean/heterousian clergy (Aetius, Eunomius) who hold Christ is unlike the Father in substance are condemned by the same homoiousian councils that reject Nicene language. The compromise formula excludes them from the settlement it builds, treating their position as the other error to be fenced out.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, strict_arian_clergy, payer,
    moderate, civilizational, constrained, continental).

% The Cappadocian-brokered homoousios settlement ratified at Constantinople 381 is not yet the standard during the homoiousian period, but the compromise's own moderate wing (Basil of Caesarea among its heirs) is largely absorbed into it afterward. It has no seat at the tables that produce the homoiousian formula and only retroactively supersedes it.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, later_pro_nicene_synthesis, excluded,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(homoousios_christology__semi_arian_reading, later_pro_nicene_synthesis).

% Later ecclesiastical historians (Socrates Scholasticus, Sozomen) and modern scholars reconstruct the sequence of councils and formulas, assessing homoiousios as a genuine attempt at doctrinal peace that ultimately could not hold against the more precise Nicene settlement.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal formula broad enough that bishops who reject both strict Nicene identity-language and strict Arian subordination can share communion and imperial favor without immediate mutual anathema, holding the eastern church together during a period when neither extreme commanded a stable majority.
% TRANSFER_FUNCTION: Moves ecclesiastical legitimacy and imperial backing toward the moderate eastern episcopate and away from both Athanasian Nicenes and strict Anomoean Arians; moves the cost of exclusion onto whichever wing the sitting emperor currently disfavors.
% ABSENT_VOICES: The Cappadocian pro-Nicene synthesis that will eventually absorb this formula's moderate wing has no voice in the 350s councils that produce homoiousios; likewise Western Latin bishops largely outside this dispute are not parties to the eastern compromise that claims to speak for the whole church.
% DISAPPEARANCE_RATIONALE: Had the homoiousian formula not existed as a coordinating middle position, the mid-4th-century eastern church would have lacked a workable compromise language between Nicene and Arian factions during the period of contested imperial favor, likely producing earlier, harder schism rather than the eventual absorption into the 381 settlement.
% FOUNDING_PROBLEM: The Council of Nicaea (325) settled homoousios on paper but left decades of unresolved factional conflict between subordinationist and identity-of-substance readings of Christ's divinity, with no stable imperial or conciliar consensus; homoiousios was built to give the contested eastern middle a formula it could actually hold to.
% FOUNDING_PROBLEM_CORROBORATION: The Council of Constantinople (381), convened under Theodosius I, formally re-affirmed homoousios and effectively closed the compromise question the homoiousian formula existed to manage; Cappadocian theologians who had been sympathetic to homoiousian language (notably Basil of Caesarea) themselves moved to the homoousian settlement, corroborating from within the moderate camp itself that the founding problem had been resolved by absorption rather than by the compromise formula's own endurance.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38) and rises through the Sirmium/Seleucia/Constantinople councils of 357-360 (peak imperial enforcement of the compromise against both wings) before easing as Constantius's death (361) and later imperial indifference reduce the formula's coercive backing; theater_ratio and suppression_requirement track the same arc — synodal condemnation and deposition activity (the suppressive machinery) intensifies exactly when imperial patronage is most invested in making the compromise the sole legal formula, then relaxes as the formula's political sponsors lose power and its function is progressively absorbed rather than defended. accessibility_collapse (0.4) and resistance (0.55) are authored moderate: unlike a mountain, real alternative formulas (homoousios, anomoios) remain visibly available and actively argued throughout the interval — the compromise never fully collapses the alternative space, and it meets sustained resistance from both flanks it was built to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   From the eastern moderate agenda-setting seat, the formula is a genuine, hard-won coordination achievement holding the church together under difficult political conditions. From the strict Nicene seat, the same formula is the mechanism of their exile and exclusion from communion — a tangled-rope-flavored experience even though this story's own claimed_type is scaffold. The engine computes these divergent seat-level readings from the structural data; the claimed_type here states what the eastern moderates' own coordination framing asserts, not an adjudication between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern episcopal moderates and the imperial court sit near the beneficiary end: they set the formula's terms and collect ecclesiastical/political stability from its operation. Unity-seeking congregations benefit incidentally (less local disruption) without administering anything. Strict Nicene theologians and strict Arian clergy sit near the target end: both bear exile, deposition, or exclusion from communion as the direct cost of the compromise holding — their trapped/constrained exit options (episcopal office and communion status are not portable outside the imperial church) push their effective extraction upward despite the formula's own moderate self-description.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a fractured, unstable eastern episcopate lacking any formula able to command a working majority — is genuinely resolved by 381, but not by the homoiousian formula's own survival: it is resolved by absorption into the Cappadocian pro-Nicene synthesis, whose homoousios language the moderate wing (Basil of Caesarea) itself comes to accept. This prevents misreading homoiousios as either pure extraction (it did solve a real coordination problem for two decades) or as permanently vindicated coordination (its own beneficiaries abandoned it once a better-specified formula solved the same problem more durably) — the sunset is real, but it arrives by succession rather than by a declared clause internal to the formula itself, which is why has_sunset_clause is authored true against the scaffold claim rather than against any textual sunset provision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_vs_genuine_middle_position,
    'Was homoiousios a sincere, theologically substantive middle position, or a deliberately vague formula whose imprecision was the coordination mechanism itself — i.e., was ambiguity the feature, not a bug?',
    'Close textual analysis of homoiousian conciliar acts and letters (Basil of Ancyra''s synodal letter, Sirmium 357/359 formulas) against later Cappadocian clarifications of ousia vs. hypostasis; comparison with how quickly and how much theological content each side attributes to the term in contemporaneous polemics.',
    'If the ambiguity was strategic, the coordination function is better described as a stability-buying device (closer to scaffold/tangled_rope) than as an independent theological achievement; if sincere, it is better read as a good-faith rope that failed on the merits rather than on its coordination design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_vs_genuine_middle_position, conceptual, 'Whether the compromise''s vagueness was deliberate coordination strategy or genuine theological uncertainty.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the disagreement between the three kernel readings (arian, semi_arian, pro_nicene) live — is it a genuine metaphysical dispute about substance-identity, or a dispute over which Greek philosophical vocabulary (ousia, hypostasis, homoios) can bear the theological weight required, with the underlying commitment to Christ''s full divinity shared by semi_arian and pro_nicene factions but not by strict arian?',
    'This is the committer-structure content required by Rule 2 of the kernel protocol — it is not resolvable within this single reading''s story and is recorded here rather than folded into ε or the claimed_type.',
    'If the dispute is primarily terminological (as many patristic scholars now argue for homoousios vs. homoiousios specifically), this reading''s absorption into pro_nicene_reading is a convergence of substance masked by vocabulary; if the dispute is genuinely metaphysical, the absorption is better read as one faction''s defeat rather than a terminological reconciliation. Either resolution changes how the two readings'' reading_relations edge (influences vs. a stronger claim) should be weighted downstream.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates where in the kernel the three readings actually disagree — terminology or metaphysics.').

omega_variable(
    imperial_patronage_vs_theological_merit,
    'Did homoiousios persist through the 350s because of genuine conciliar consensus among eastern bishops, or primarily because Constantius II''s imperial machinery enforced it as the legally favored formula — i.e., is the measured suppression_requirement rise in 357-360 evidence that the formula''s apparent dominance was substantially manufactured by state coercion rather than won theologically?',
    'Comparative analysis of conciliar attendance and signatory patterns under imperial pressure (forced signatures at Sirmium, exile threats) versus voluntary theological alignment recorded in independent correspondence (e.g., letters of bishops not directly subject to imperial summons).',
    'If largely imperially manufactured, the formula''s coordination-function claim weakens considerably and the constraint reads closer to tangled_rope (coordination cover for imperial control over episcopal appointments) than to a scaffold genuinely built and sustained by the coordinating parties themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_patronage_vs_theological_merit, empirical, 'Whether the formula''s apparent 350s dominance reflects genuine consensus or imperial coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 342, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t342, homoousios_christology__semi_arian_reading, theater_ratio, 342, 0.18).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__semi_arian_reading, theater_ratio, 350, 0.22).
narrative_ontology:measurement(homo_tr_t357, homoousios_christology__semi_arian_reading, theater_ratio, 357, 0.28).
narrative_ontology:measurement(homo_tr_t360, homoousios_christology__semi_arian_reading, theater_ratio, 360, 0.34).
narrative_ontology:measurement(homo_tr_t366, homoousios_christology__semi_arian_reading, theater_ratio, 366, 0.31).
narrative_ontology:measurement(homo_tr_t375, homoousios_christology__semi_arian_reading, theater_ratio, 375, 0.29).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__semi_arian_reading, theater_ratio, 381, 0.3).

% Extraction over time
narrative_ontology:measurement(homo_be_t342, homoousios_christology__semi_arian_reading, base_extractiveness, 342, 0.22).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__semi_arian_reading, base_extractiveness, 350, 0.28).
narrative_ontology:measurement(homo_be_t357, homoousios_christology__semi_arian_reading, base_extractiveness, 357, 0.35).
narrative_ontology:measurement(homo_be_t360, homoousios_christology__semi_arian_reading, base_extractiveness, 360, 0.42).
narrative_ontology:measurement(homo_be_t366, homoousios_christology__semi_arian_reading, base_extractiveness, 366, 0.4).
narrative_ontology:measurement(homo_be_t375, homoousios_christology__semi_arian_reading, base_extractiveness, 375, 0.36).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__semi_arian_reading, base_extractiveness, 381, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t342, homoousios_christology__semi_arian_reading, suppression_requirement, 342, 0.25).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__semi_arian_reading, suppression_requirement, 350, 0.32).
narrative_ontology:measurement(homo_su_t357, homoousios_christology__semi_arian_reading, suppression_requirement, 357, 0.4).
narrative_ontology:measurement(homo_su_t360, homoousios_christology__semi_arian_reading, suppression_requirement, 360, 0.48).
narrative_ontology:measurement(homo_su_t366, homoousios_christology__semi_arian_reading, suppression_requirement, 366, 0.44).
narrative_ontology:measurement(homo_su_t375, homoousios_christology__semi_arian_reading, suppression_requirement, 375, 0.4).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__semi_arian_reading, suppression_requirement, 381, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the homoousios_christology kernel (the substance-relation of Christ to the Father as contested from Nicaea 325 through Constantinople 381). semi_arian_reading (this story) authors moderate ε (0.38) and an explicit coordination function, historically absorbed into pro_nicene_reading after 381. pro_nicene_reading authors the eventually-victorious homoousios settlement with its own independent ε and enforcement structure (higher institutionalized enforcement post-381, backed by imperial orthodoxy law). arian_reading authors the subordinationist position this reading and pro_nicene_reading both exclude, with its own ε reflecting its post-381 illegalization. Each story's ε is stable and referent-fixed to its own reading's standing arrangement; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

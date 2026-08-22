% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: Prophetic Suspension of Plural Marriage (Endogenous Reinterpretation Reading)
 *   domain: religious/institutional/political-theology
 *
 * SUMMARY:
 *   The 1890 Manifesto announcing the church's suspension of plural marriage
 *   practice is here interpreted as endogenous prophetic reinterpretation—God
 *   revealed the temporal suspension to preserve the church's salvific
 *   mission and institutional survival. This reading frames the constraint as
 *   rope (coordination around a new prophetic directive that aligns
 *   membership practice with political reality while retaining doctrinal
 *   coherence). The constraint's beneficiaries are the church institution
 *   (gains survival, mainstream legitimacy, temple recognition, missionary
 *   access) and mainstream membership (gains civic participation without
 *   stigma). The victims are fundamentalist adherents who maintain the
 *   original reading and are excommunicated for it. This is ONE READING of
 *   the contested plural_marriage_mandate kernel; the exogenous-override
 *   reading and institutional-pragmatism reading instantiate different
 *   constraints from the same kernel by reframing causation (divine vs.
 *   federal coercion) and legitimacy (genuine reinterpretation vs.
 *   survival-driven cover story).
 *
 * KEY AGENTS:
 *   - Church Institution (institutional power, arbitrage exit): sets and enforces the new reading via hierarchy; gains survival and mainstream access.
 *   - Mainstream Membership (organized power, constrained exit): benefits from ability to practice the faith without legal persecution or social stigma.
 *   - Fundamentalist Adherents (powerless, identity-locked exit): bear the cost of excommunication for maintaining original reading; their identity is fused with the original doctrine.
 *   - Federal Government (institutional power, excluded): applied material coercive pressure (Edmunds Acts); systematically backgrounded in this reading's framing.
 *   - Reformist Intellectuals (moderate power, mobile exit): benefit by becoming interpreters of progressive revelation; gain authority from alignment with institutional position.
 *   - Splinter Sects (powerless, trapped exit): maintain original reading, lose institutional resources and legitimacy.
 *   - Academic Observers (analytical, neutral): analyze the reading's structural coherence against historical evidence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.22).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "Prophetic Suspension of Plural Marriage (Endogenous Reinterpretation Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/institutional/political-theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'b5d3d0d6-7302-461c-9e9d-35e9e908e92e').
narrative_ontology:cs_kernel_codification('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', fixed_text).
narrative_ontology:cs_authority_grounding('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', lineage).
narrative_ontology:cs_interpretation_layer_present('b5d3d0d6-7302-461c-9e9d-35e9e908e92e').
narrative_ontology:cs_reading_relation('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', foundational, prophetic_revelation_endogenous).
narrative_ontology:cs_axiom_status(prophetic_revelation_endogenous, holdable).
narrative_ontology:cs_axiom_grounding('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', prophetic_revelation_endogenous, theological).
narrative_ontology:cs_axiom('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', foundational, doctrine_practice_coherence_sustainable).
narrative_ontology:cs_axiom_status(doctrine_practice_coherence_sustainable, holdable).
narrative_ontology:cs_axiom_grounding('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', doctrine_practice_coherence_sustainable, deontological).
narrative_ontology:cs_reference_frame('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', perpetual_prophetic_authority).
narrative_ontology:cs_drift_state('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', contemporary_post_1980s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5d3d0d6-7302-461c-9e9d-35e9e908e92e', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, missionary_arm).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, reformist_intellectuals).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Salt Lake–based hierarchical leadership (First Presidency, Quorum of Twelve Apostles) announces and enforces the 1890 Manifesto suspending plural marriage practice while retaining the doctrine as eternally binding. They frame this as prophetic reinterpretation—God revealed the temporal suspension to preserve the church's institutional survival, temple access legitimacy, and missionary expansion into mainstream American society. They administer temple recommend interviews (gate-keeping) to enforce compliance and excommunicate those who continue the practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain ability to exist within mainstream American society, reduce legal persecution and property seizure, expand missionary recruitment (converts avoid association with polygamy), participate in civic life without suspicion, and access the institutional church's religious services, temple rites, and social belonging. They also carry a diffuse indirect cost: the doctrinal incoherence of retaining plural marriage as eternally binding while indefinitely suspending practice creates theological strain they must continually reconcile. The secondary payer role reflects this dual position.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership, payer).

% Believe plural marriage was an eternal divine ordinance, not temporally suspendable. They experience the Manifesto as betrayal of revealed truth. Continuing the practice results in excommunication—loss of temple access, ecclesiastical standing, community belonging, ritual participation, and often family connection (as the institutional church shuns them). Their exit from the practice requires rejecting their core religious identity; exit from the church means losing their entire cosmological and social framework. Identity-lock is the constraint's mechanism of victimization.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_adherents, payer,
    powerless, biographical, identity_locked, local).

% Applied sustained legal and economic pressure (Edmunds Acts, Edmunds-Tucker Act, property seizures, imprisonment of church leaders) to force abandonment of plural marriage. This reading treats the Manifesto as endogenous (prophetic reinterpretation) rather than exogenous (federal coercion), so federal power is systematically absent from the framing, though historically it was the material constraint that made institutional survival depend on abandonment. Federal government would object to the endogenous framing if present in the conversation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, trapped, national).

% Church members and scholars who had begun arguing for reinterpretation of plural marriage within progressive-revelation doctrine. The Manifesto validates their theological framing and elevates them to prominence in institutional discourse (they become the interpreters of how doctrine can evolve). They gain authority to articulate why the church can claim doctrinal fidelity while suspending practice. Mobile exit allows them to leave institutional positions if the theology fails; they choose to stay because the reading aligns with their intellectual position.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, reformist_intellectuals, beneficiary,
    moderate, biographical, mobile, national).

% Communities that split off and maintained plural marriage as binding doctrine. They contest the legitimacy of the endogenous-reinterpretation reading and hold the exogenous-override reading instead (the Manifesto was coercion, not revelation). They remain excommunicated and outside institutional resources. Generational time-horizon reflects their durability as counter-communities; trapped exit reflects that splitting off did not provide a path out of the plural-marriage commitment.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, splinter_sects, excluded,
    powerless, generational, trapped, local).

% Scholars of religious history, institutional change, and political theology analyze the Manifesto as a case of constraint reinterpretation under duress. They examine whether the reading's structural claims (prophetic legitimacy, doctrinal coherence, endogenous authority) are consistent with institutional practice and historical evidence. Analytical position allows them to remain outside all three readings while analyzing their structural relationships.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles the institutional church's claim to prophetic continuity and doctrinal immutability with the political requirement to abandon a practice that made survival impossible. The coordination solved is: membership in a church that remains doctrinally coherent (plural marriage still eternally binding) while conforming to external political reality (practice suspended). Without this reinterpretation, the church faced either institutional dissolution or open defiance of federal law leading to collapse.
% TRANSFER_FUNCTION: Moves excommunication and loss of temple access (and community belonging) FROM mainstream members complying with the Manifesto TO fundamentalist adherents who reject it. The constraint transfers religious standing and institutional legitimacy: those who accept the endogenous-reinterpretation reading retain full participation; those who reject it lose everything. The church institution gains survival, mainstream legitimacy, and expanded missionary access.
% ABSENT_VOICES: Federal government coercive pressure is systematically absent from this reading's framing (it is centered in the exogenous-override reading instead). Fundamentalist adherents who would reject the reading are excluded via excommunication and structured out of institutional discourse. Splinter sects that maintain the original doctrine are marginalized and lack institutional platform.
% DISAPPEARANCE_RATIONALE: If the Manifesto and the endogenous-reinterpretation reading disappeared—if the church had instead maintained plural marriage as binding practice—the institution would have faced either legal destruction (further prosecutions, property seizures leading to institutional insolvency), schism (mainstream members departing to save themselves), or open theocratic defiance with military consequence. The 1890 suspension enabled the church's integration into American political and social life; its disappearance would have restructured the entire American religious landscape and the church's institutional trajectory.
% FOUNDING_PROBLEM: The church's foundational theological claim includes plural marriage as an eternally binding divine ordinance; federal law made maintaining the practice impossible without institutional collapse. The founding problem of this constraint is: how can a church claim prophetic authority and doctrinal continuity while suspending a teaching it previously declared eternally binding?
% FOUNDING_PROBLEM_CORROBORATION: The church hierarchy attests the founding problem as solved through prophetic revelation (the endogenous-reinterpretation reading). Fundamentalist sects and academic historians outside the institutional church attest that the founding problem was NOT solved through reinterpretation but through coercion—federal pressure forced abandonment, and the revelation narrative is ex-post-facto legitimation (exogenous-override reading). Legal historians and secular scholars document the material coercive machinery (Edmunds-Tucker Act, property seizures, imprisonment) that preceded the Manifesto, supporting the contested status.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 end-state) because the constraint coordinates genuine membership benefit (avoiding persecution) with institutional extraction (control over doctrinal interpretation and excommunication authority). Early extractiveness is lower (0.18 at t=0) because immediately post-Manifesto, the reading is framed as received revelation and widely accepted; extraction accumulates (reaching 0.38 by t=12 and plateauing) as the gap between official doctrine (plural marriage eternally binding) and actual practice (suspended indefinitely) becomes historically visible and requiring active enforcement to maintain. Suppression starts low (0.08) and rises gradually (reaching 0.22 by t=12) as enforcement machinery strengthens: temple-recommend interviews intensify, excommunication boundaries harden, splinter sects are more aggressively marginalized. Theater ratio rises from 0.28 to 0.41 as the reading itself becomes increasingly theatrical—the official doctrine (plural marriage eternally binding) becomes increasingly disconnected from the coordination function actually being performed (mainstream respectability, temple legitimacy). The measurement series share one time grid: every metric is authored at every examined point (t=0, 4, 8, 12, 20, 40) so temporal analysis can detect where theater overtakes function.
 *
 * PERSPECTIVAL GAP:
 *   From the church institution's seat, the Manifesto is genuine reinterpretation—a new revelation showing how doctrine and practice can diverge temporally while remaining doctrinally coherent. From the fundamentalist seat, the same constraint is coerced abandonment of a divine requirement, and the revelation narrative is ex-post-facto cover story. From the federal government's seat (excluded from this reading), the constraint is successful coercive pressure. The engine computes per-seat classifications from the structural data; this reading's chair privileges the institutional perspective by framing the founding problem as doctrinal (how to maintain coherence while suspending practice) rather than political (how to preserve the church under federal assault). The exogenous-override reading privileges the federal-coercion perspective and reframes extractiveness accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institution is the structural beneficiary and agenda-setter (collects institutional survival, temple legitimacy, missionary access—d near 0.0-0.2). Mainstream membership is near-symmetric (genuine coordination benefit from avoiding persecution + diffuse indirect cost of doctrinal incoherence—d near 0.4-0.6). Fundamentalist adherents are the structural targets (bear excommunication, loss of temple, identity dissolution—d near 0.85-1.0). The reading's core claim is that this extraction is legitimate because it flows from prophetic authority, not from coercive override. The engine computes directionality from beneficiary/victim + exit; the commentary explains why the victims remain trapped (identity-locked exit prevents departure without cosmological dissolution).
 *
 * MANDATROPHY ANALYSIS:
 *   The 1890 Manifesto presents a potential mandatrophy case: the founding problem (how to maintain institutional survival while preserving doctrinal coherence) might become obsolete once institutional integration succeeds. However, the reading claims the founding problem remains live—plural marriage remains eternally binding doctrine even with indefinitely suspended practice, and the church must continue asserting this coherence to maintain its theological legitimacy. The contested status of founding_problem_status reflects this: the institutional reading says the founding problem (doctrinal coherence under pressure) is perpetually live and perpetually solved by prophetic authority; the fundamentalist reading says the founding problem was the false framing all along—the real problem was federal coercion, which the Manifesto did not solve, only capitulated to. If the institutional reading's founding problem becomes demonstrably dead (the church formally abandons the doctrine, or declares it null), mandatrophy resolution would follow. Currently the constraint remains classified as rope under this reading because the coordination function (maintain institutional membership while suspending practice) remains active and the beneficiaries (mainstream membership, church institution) remain organized and defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_causation,
    'Was the 1890 Manifesto precipitated endogenously (God revealed the suspension to preserve the church''s mission) or exogenously (federal coercion forced abandonment, and the revelation narrative is post-hoc legitimation)?',
    'Historical counterfactual analysis: would the church have voluntarily adopted the suspension absent federal legal pressure? Documentary evidence from church leadership private communications, contemporaneous rival accounts from fundamentalist and federal actors, and comparative analysis of other religious institutions facing similar coercive pressure.',
    'If revelation-driven: the constraint remains rope (coordination around a new prophetic directive), extractiveness is moderate, and the reading holds. If coercion-driven: the constraint reclassifies to snare or tangled-rope under the exogenous-override reading, extractiveness rises, and the endogenous-reinterpretation reading becomes a false-summit candidate (beneficiaries using natural-law framing to defend extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_vs_revelation_causation, conceptual, 'Whether the Manifesto''s causation is divine (this reading) or coercive (sibling reading).').

omega_variable(
    doctrine_practice_coherence,
    'Can a commitment system legitimately retain an eternally binding doctrine in an indefinitely suspended form, or does indefinite suspension constitute de-facto abandonment regardless of rhetorical coherence?',
    'Comparison with other religious and legal commitments that claim temporal suspension of binding doctrine (e.g., Jewish Sabbatical year observance in diaspora, Islamic jurisprudential categories of suspended obligations). Examination of whether the church''s post-1890 institutional practice treats plural marriage as live doctrine or as historical artifact rhetorically retained for coherence.',
    'If coherence is sustainable: the rope classification holds, theater_ratio at 0.41 is acceptable (some performative function is inherent to maintaining doctrine-practice gaps). If coherence fails: theater_ratio rises toward 0.7+ (constraint becomes mostly theatrical maintenance), and constraint type reclassifies toward piton (institutionally maintained but functionally atrophied).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_coherence, conceptual, 'Whether indefinite suspension of eternally binding doctrine is coherent or a failed commitment.').

omega_variable(
    identity_lock_durability,
    'Do fundamentalist adherents remain identity-locked to the original plural-marriage reading, or does the institutional church''s sustained reinterpretation eventually shift the landscape such that a new identity (accepting the suspension as legitimate reinterpretation) becomes normative and the original identity becomes merely historical?',
    'Generational tracking of fundamentalist sect membership, splinter-sect member exit patterns, genealogical studies of which descendants of polygamists maintain the original reading vs. accept the institutional reinterpretation, and qualitative interviews with splinter-sect members about the phenomenology of identity-lock.',
    'If identity-lock persists: fundamentalist adherents remain trapped victims (high extractiveness for this population), splinter sects remain structurally excluded, and constraint persistence depends on ongoing excommunication machinery. If identity-lock weakens: the victim population shrinks, theater_ratio rises (the constraint becomes mostly intra-institutional doctrinal maintenance), and constraint might reclassify toward piton or rope-degraded as the enforcement function becomes theatrical rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Whether fundamentalist identity-lock to the original reading persists or erodes over generations.').

omega_variable(
    kernel_contest_framing,
    'Is the reading difference between endogenous-reinterpretation, exogenous-override, and institutional-pragmatism framings a matter of legitimate alternative interpretations of the same facts, or does one framing systematically obscure the facticity of the others (e.g., the endogenous framing obscures federal coercion by design)?',
    'Systematic examination of what facts each reading is able to accommodate and what facts each reading must suppress. Comparison of the three readings'' handling of: (a) federal legislative history and enforcement machinery, (b) internal church documents showing leadership concern about survival, (c) the timing of the Manifesto relative to coercive pressure, (d) fundamentalist accounts of their own experience.',
    'If all three readings are equipoised (each accommodates its core facts and suppresses others symmetrically): the constraint remains contested, and all three readings remain live. If one reading systematically suppresses facticity that the others accommodate: that reading becomes a false-summit candidate (beneficiaries using selective framing to defend extraction), and reclassifies toward snare under challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether the three readings are symmetrically suppressional or whether one achieves better facticity accommodation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(plur_tr_t4, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(plur_tr_t8, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(plur_tr_t12, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(plur_tr_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(plur_tr_t40, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(plur_be_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(plur_be_t4, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(plur_be_t8, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(plur_be_t12, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(plur_be_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(plur_be_t40, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(plur_su_t4, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement(plur_su_t8, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 8, 0.16).
narrative_ontology:measurement(plur_su_t12, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 12, 0.19).
narrative_ontology:measurement(plur_su_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(plur_su_t40, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the plural_marriage_mandate kernel. The exogenous_override_reading and institutional_pragmatism_reading are sibling constraints instantiated by the same kernel but with different framing of causation (divine reinterpretation vs. federal coercion vs. pragmatic cover story) and legitimacy. Each reading produces a structurally different constraint with different beneficiary/victim sets, extraction profiles, and theater ratios. The readings coexist as live positions across institutional factions and remain unresolved. Network links enable the corpus to model kernel-level contests as constraint families rather than as single ambiguous constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__endogenous_reinterpretation_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

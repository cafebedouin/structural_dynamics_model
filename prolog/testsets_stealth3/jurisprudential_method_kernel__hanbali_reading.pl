% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Textualist Exclusivity Norm (Hanbali Reading of Jurisprudential Method)
 *   domain: religious/legal-institutional
 *
 * SUMMARY:
 *   Within the legal field governed by the Hanbali reading, valid derivation
 *   of law admits only the revealed text, transmitted Companion opinion, and
 *   agreement that is genuinely universal among qualified jurists; systematic
 *   inference from texts and discretionary preference among readings are
 *   classified as religious innovation, and their practitioners as corrupters
 *   of the inherited kernel. Enforcement runs through certification of
 *   teachers and judges, refutation literature, and social sanction up to
 *   refusal of association. KEY AGENTS (by structural relationship):
 *   hanbali_school_authorities — agenda-setting certifiers
 *   (institutional/identity_locked) who administer the norm and collect its
 *   institutional rents; hadith_textualist_scholars — primary beneficiaries
 *   (organized/identity_locked) whose careers ride on transmission expertise;
 *   rationalist_jurists — primary targets (organized/constrained) whose
 *   working tools are condemned; customary_practice_communities — secondary
 *   targets (powerless/trapped) whose practices lose standing; muslim_public
 *   — dual-positioned recipients of predictability and payers of rigidity;
 *   rival_school_jurists — excluded parties whose methods are ruled out at
 *   the threshold; juridical_methodologists — analytical observers. This file
 *   is ONE READING of the four-reading jurisprudential-method kernel: the
 *   colloquial label 'Islamic legal methodology' covers four structurally
 *   distinct source hierarchies with different epsilon values, so per the
 *   decomposition principle they are authored as four linked stories (network
 *   edges below), not one story with a measurement parameter. The epsilon
 *   referent here is the standing textualist-exclusive arrangement assessed
 *   from this reading's analytic seat — high, because the arrangement
 *   forecloses an entire class of reasoning tools and strips customary
 *   practice of standing, while retaining a real coordination core. Claimed
 *   type and metrics are authored independently: I believe the structure is a
 *   hybrid of genuine coordination and asymmetric extraction actively
 *   enforced, and I have authored the metrics I believe descriptively true of
 *   its actual operation; where computed per-seat classifications diverge
 *   from the claim, that divergence is the measurement.
 *
 * KEY AGENTS:
 *   - hanbali_school_authorities: agenda-setting certifiers (institutional/identity_locked) — declare innovation, license teachers and judges, collect the institutional rents
 *   - hadith_textualist_scholars: primary beneficiaries (organized/identity_locked) — careers and prestige flow through transmission mastery
 *   - rationalist_jurists: primary targets (organized/constrained) — inferential tools condemned, standing contingent on affiliation switching
 *   - customary_practice_communities: secondary targets (powerless/trapped) — generational customs voided unless textually anchored
 *   - muslim_public: dual-positioned (moderate/constrained) — receives source-traceable law, pays in rigidity on novel questions
 *   - rival_school_jurists: excluded (powerful/mobile) — methods ruled out at the threshold rather than engaged
 *   - juridical_methodologists: analytical observers — compare validation standards across schools, collect nothing, rule on nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.72).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.6).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Textualist Exclusivity Norm (Hanbali Reading of Jurisprudential Method)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal-institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '172e99a5-c192-44ad-a7d9-ab0dbfe372b6').
narrative_ontology:cs_kernel_codification('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', fixed_text).
narrative_ontology:cs_authority_grounding('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', lineage).
narrative_ontology:cs_interpretation_layer_present('172e99a5-c192-44ad-a7d9-ab0dbfe372b6').
narrative_ontology:cs_reading_relation('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', jurisprudential_method_kernel__maliki_reading, influences).
narrative_ontology:cs_axiom('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', foundational, exclusive_revelatory_derivation).
narrative_ontology:cs_axiom_status(exclusive_revelatory_derivation, holdable).
narrative_ontology:cs_axiom_grounding('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', exclusive_revelatory_derivation, deontological).
narrative_ontology:cs_axiom('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', foundational, analogical_reasoning_corrupts_kernel).
narrative_ontology:cs_axiom_status(analogical_reasoning_corrupts_kernel, overridden).
narrative_ontology:cs_axiom_grounding('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', analogical_reasoning_corrupts_kernel, instrumental).
narrative_ontology:cs_reference_frame('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', salaf_textual_exclusivity).
narrative_ontology:cs_drift_state('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', late_classical_post_taymiyyan, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('172e99a5-c192-44ad-a7d9-ab0dbfe372b6', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_school_authorities).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hadith_textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, muslim_public).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, muslim_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior jurists who certify what counts as valid derivation of law: they declare which methods constitute innovation, license teachers and judges, and preside over the transmission chains through which texts and Companion opinions circulate. Appointments, endowments, and deference flow through their offices. The exclusivity of the method is the basis of the office they hold; revising it from within is possible — later masters did — but revision dissolves the distinct identity that anchors their standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_school_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).

% Scholars whose careers rest on mastery of transmitted reports, isnad criticism, and Companion precedent. Prestige, students, and preferment concentrate on those with transmission expertise rather than dialectical training. Their scholarly self-concept is built through the transmission community; setting the method aside would mean discarding a lifetime's accumulation of memorized material and their place in the chain.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hadith_textualist_scholars, beneficiary,
    organized, biographical, identity_locked, continental).

% Jurists formed in dialectical theology who extend rulings to novel cases by systematic inference and by weighing which of two admissible readings better serves the law's evident aims. Under the exclusivity norm their working tools are condemned as corruption; they face refutation treatises, loss of teaching posts, and refusal of association from stricter colleagues. Affiliation with a rival school where inference is a sanctioned tier remains open, at the price of retraining and surrendered standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    organized, biographical, constrained, continental).

% Market towns, trade networks, and village communities whose settled customs govern contracts, dowry adjustments, and penalties. The norm grants their practices no standing unless anchored in explicit text or demonstrably universal agreement, so arrangements maintained for generations can be voided by a judge applying the letter of a report. They cannot exit their own practices; abandonment of custom or litigation across schools is the only recourse, and neither is realistically available to most.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    powerless, generational, trapped, regional).

% Litigants and households who receive law anchored in recognizable sacred sources — decisions traceable to a text rather than to an individual jurist's preference, and therefore harder for a governor or judge to bend. They pay in rigidity: new commercial instruments, mixed marriages, fiscal novelties, and medical questions go unresolved or are resolved against local practice. Their influence is exercised by petitioning scholars of whichever school administers their locale.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, muslim_public, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, muslim_public, payer).

% Jurists of the analogical and Medinan-tradition schools, who hold chairs, judgeships, and students in the same cities and whose methods are ruled out at the threshold rather than engaged. Under the exclusivity norm their tools are defined as corruption before any debate begins, so their methodological objections register only as confessional polemic. They operate fully within their own institutions and suffer the norm chiefly where it controls appointments.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rival_school_jurists, excluded,
    powerful, biographical, mobile, continental).

% Specialists in the theory of legal sources and chroniclers of the schools' disputes, who compare how each method validates what it counts as evidence and where each stalls on novel cases. They collect no fees from the arrangement and render no rulings under it; their assessments circulate in teaching circles and shape reputations at one remove.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, juridical_methodologists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, hanbali_school_authorities).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, predictable standard for deriving law from revelation: every trained jurist draws on the same texts and the same transmitted opinions, so rulings are traceable to common sources rather than to each judge's private reasoning, and the community's legal identity stays anchored to its founding materials.
% TRANSFER_FUNCTION: Moves juridical authority, teaching posts, and certification power from jurists credentialed in reasoning techniques toward jurists credentialed in text mastery and transmission chains; moves adjudicative predictability to the public; and strips customary arrangements of official standing unless they can show textual or universal attestation.
% ABSENT_VOICES: Rationalist jurists and the communities governed by entrenched custom would object that systematic inference is how revelation reaches novel cases and that long-standing local practice embodies accumulated need — but both stand outside the school's certification process. Their objections arrive only as charges of innovation to be answered, never as counterproposals with a seat; the rival-school jurists who could press them are defined out of the conversation at the threshold.
% DISAPPEARANCE_RATIONALE: If the exclusivity norm vanished overnight, jurists would resume systematic inference and discretionary weighting without censure, customary arrangements would regain standing as adjudicable sources, school boundaries would blur toward a unified science of legal sources, and the certification apparatus that distributes teaching posts and judgeships would lose its basis — the whole economy of juridical authority would reorganize.
% FOUNDING_PROBLEM: Keep law faithful to its revealed sources when juristic reasoning and imperial politics both push toward discretionary reinterpretation. The immediate crucible was the state-sponsored theological inquisition of the ninth century, under which spokesmen for transmitted text were pressured to defend scripture with the interrogators' own speculative tools; the enduring formulation was broader: prevent human invention from being passed off as religion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the benefiting parties by contemporaneous trial records and chronicles of the inquisition-era prosecutions of traditionists, which no school faction authored. The opposing reading — that the acute fidelity crisis ended centuries ago and the arrangement now mainly protects interpretive monopoly — is attested by rival-school polemics and by internal critics within the tradition itself who argued that unrestricted condemnation of inference had become a weapon in career disputes. Both attestations exist; neither comes from the beneficiary set alone.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the norm forecloses a whole class of legitimate reasoning tools and strips customary practice of standing — the transfer runs in authority and adjudicable legitimacy, not money. Suppression (0.60) is substantial but below the extractiveness: enforcement is real (certification control, refutation campaigns, social boycott) yet partly self-executing through trained self-limitation, which the omega on suppression mechanisms isolates. Theater (0.28) rises across the interval: early condemnations answered live disputes; by the late period, declaring innovation increasingly functioned as boundary-marking performance that outlasted the disputes it once adjudicated — a symptom worth watching, not the classification test. Accessibility collapse (0.60) reflects a split reality: within the school's jurisdiction, alternatives collapse almost completely once the norm is understood (inferential rulings simply cannot be certified), but rival schools persist externally, so global alternatives never vanish. Resistance (0.55) stayed high throughout: rationalist jurists never stopped inferring, custom-holders kept litigating around the norm, and the tradition produced its own internal critics — the constraint had to be defended continuously. The measurement series share one grid (T=0..60) and tell a three-phase arc: moderate extraction in the defensive founding phase (when textual loyalty protected a persecuted minority and cost fell mainly on internal speculation), rising extraction as the school institutionalized and policing boundaries became career infrastructure, peak around mid-interval, then decline as the tradition's own towering late figure argued that restricted inference was unavoidable — enforcement machinery decayed while nominal condemnations persisted, which is why suppression falls faster than theater rises at the tail. The suppression_requirement series is authored deliberately because this story specifically tracks enforcement-capacity build-up and decay; a static scalar would hide the arc. Coalition note: the two target groups never cohered — rationalist jurists were an urban professional class whose interests lay in method monopoly elsewhere, and custom communities had no seat in methodological debate — so diffuse victimhood did not convert into joint leverage. Identity-lock dynamics: both the certifying authorities and the transmission scholars are identity_locked; the fusion is professional and relational (a lifetime's memorized material and standing in the chain), so even costless exit would feel like apostasy from one's own life's work; if that frame broke, certification power would dissolve quickly, since the norm's enforcement depends on volunteers who believe it.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the certifying authorities' seat the arrangement is an obligation they administer and a trust they keep — near the coordination end, with extraction experienced as devotion. From the transmission scholars' seat it is subsidy fused with vocation: the same structure that pays them is the one they cannot imagine leaving. From the rationalist jurists' seat the identical structure operates as delegitimation — their competence redefined as corruption, their career contingent on switching allegiance. From the custom communities' seat it is pure rigidity imposed from outside with no recourse. The public seat straddles: traceable rulings against arbitrary ones is a real gain, unanswered novel questions a real loss. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the certifying authorities derive near-full-beneficiary directionality (they collect appointments and deference through the norm they set), and transmission scholars derive low directionality amplified in stability by identity_locked exit — trapped beneficiaries are the steadiest subsidy. Rationalist jurists derive near-full-target directionality (declared victims, constrained exit: school-switching is possible but costly). Customary practice communities sit at the extreme target end: declared victims, powerless, trapped — nowhere to take practices that the norm voids. The muslim_public seat is intentionally LEFT OFF the base_properties arrays despite carrying dual beneficiary/payer roles: adding it to beneficiaries would derive a subsidized directionality and understate the rigidity costs its second role carries; omitting it lets the derivation treat the seat as near-symmetric, which matches its actual ambivalence. The excluded rival-school jurists sit outside the beneficiary/victim derivation by design — their exclusion is the enforcement object itself, not a cost-benefit position within the arrangement. Suppression, per the framework, is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope — the continental scope of scholar networks modestly amplifies effective extraction on the target seats by making verification of universal agreement harder, which feeds the unanimity-verifiability omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both mislabels. Reading this as pure extraction would erase why pious actors submitted voluntarily for centuries: the coordination function — shared, predictable, source-traceable law resistant to a judge's whim — is real, was valued by the public seat, and solved a genuine collective problem of legal legitimacy across an empire. Reading it as pure coordination would erase the destroyed careers, the voided customs, and the closed methodological conversation. The hybrid keeps both faces attached to the same structure. On the genealogy interview: founding status is contested and the disappearance verdict is world_rearranges, so the mismatch consumer finds no dead-mandate-plus-living-arrangement flag — the fidelity concern the arrangement was built for is still argued about on both sides, and the arrangement demonstrably organizes real parties. The piton path is likewise not indicated: whatever theatrical accretion the tail of the series shows, the certification function still operates and identifiable parties still profit, which places the structure firmly in hybrid territory rather than inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the hanbali reading of the jurisprudential-method kernel; how would the constraint''s structure change under a sibling reading?',
    'Comparative read of the four sibling stories (hanafi, maliki, shafii): each reverses or redraws the beneficiary/victim polarity — the hanafi reading makes reasoning jurists the subsidized party and textualists the constrained one; the shafii reading standardizes the hierarchy rather than condemning its lower tiers outright.',
    'Classification is reading-indexed: the same historical field computes with high extraction on the analogical-tool space under this reading, but with inverted directionalities under the hanafi reading. Cross-reading comparison is the corpus-level measurement; the readings are never reconciled into one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one reading of a four-reading kernel; sibling readings are separate files linked through the network edges.').

omega_variable(
    divine_warrant_vs_institutional_interest,
    'Does the exclusivity norm persist because participants experience adherence as a religious obligation (warrant experienced as natural from inside the tradition), or because it secures textualist scholars'' institutional position?',
    'Behavior under costless deviation: examine periods and settings where anonymity shielded jurists from detection, and observe whether they privately practiced inference while publicly professing exclusivity.',
    'If experienced obligation dominates, enforcement is largely self-executing and cheap, and the norm resembles an internalized duty; if institutional interest dominates, the norm requires visible enforcement machinery and behaves as constructed position-protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_warrant_vs_institutional_interest, conceptual, 'Naturality ambiguity: obligation-as-warrant versus interest-as-maintenance.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of inferential method structural (loss of posts, refutation campaigns, social boycott) or internalized (jurists who abandon inference before any sanction arrives)?',
    'Post-move trajectory of jurists who relocate to rival-school cities: if they adopt inferential method readily once outside the norm''s jurisdiction, suppression was structural; if they continue avoiding it, the inhibition internalized during formation.',
    'Internalized suppression raises effective coercion above the structural measure and outlasts the enforcement apparatus itself; purely structural suppression decays with the machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of suppression between external sanction and trained self-limitation.').

omega_variable(
    unanimity_verifiability,
    'Can the required universal agreement of qualified jurists ever be operationally verified across dispersed generations and regions?',
    'Audit of cited consensus claims: trace whether the agreements invoked were ever actually surveyed at the time or were asserted retrospectively by parties with a stake in closing a question.',
    'If universal agreement is unverifiable, the norm''s only sanctioned flexibility valve fails in practice, rigidity deepens, and measured extraction rises; if it is verifiable in restricted domains, the arrangement retains adaptive slack that dampens extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unanimity_verifiability, empirical, 'Operability of the unanimity requirement as the norm''s safety valve.').

omega_variable(
    drift_revival_cycle,
    'Is the late-classical absorption of restricted inference a permanent liberalization, or one phase of a cycle in which revival movements reimpose strict textualism?',
    'Track subsequent revival episodes — movements reasserting uncompromising anti-inference rhetoric — against the enforcement-intensity series; oscillation indicates a cycle, continued convergence indicates durable drift.',
    'A cyclical resolution predicts recurring re-exclusion of reasoning jurists and oscillating extraction; a permanent-drift resolution predicts continued convergence toward standardized method across schools.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_revival_cycle, empirical, 'Whether methodological drift oscillates through revival cycles or converges permanently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(juri_tr_t10, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(juri_tr_t30, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(juri_be_t10, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(juri_be_t30, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 50, 0.76).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(juri_su_t10, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(juri_su_t30, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic legal methodology' decomposes into four structurally distinct source hierarchies (hanafi, hanbali, maliki, shafii readings of the jurisprudential_method_kernel). Each member carries its own epsilon, beneficiary/victim polarity, and classification; they are linked through network.affects_constraints rather than merged. This member (hanbali) authors very high extraction on the analogical-tool space and links upstream to the shafii synthesis (whose hierarchy negotiated the textualist-analogist tension this reading hardened) and exerts authentication pressure on the maliki practice-doctrine; the hanafi reading is the polarity inverse of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

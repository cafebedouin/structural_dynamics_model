% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Conception-Anchored Legal Personhood Mandate
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the legal_personhood_boundary
 *   kernel: the developmental_potentiality_reading, under which legal
 *   personhood and rights-bearing status attach at fertilization and every
 *   holder of a human life trajectory is a rights-bearer. Per the
 *   committer-frame rules, the contest is not described inside the
 *   constraint: the sibling readings (functional_capacity_reading,
 *   restrictive_anthropocentric_reading) are separate constraint stories with
 *   their own epsilon values, victim sets, and classifications, linked
 *   through network.affects_constraints. The epsilon referent is this
 *   reading's own standing arrangement — the conception-anchored personhood
 *   regime as actually enacted and enforced — assessed from the analytical
 *   seat; it is not averaged against the siblings and not hedged across
 *   readings. The claim/metric gap is deliberate: the reading CLAIMS to be
 *   protective coordination (extending existing legal protection to a
 *   previously unprotected class), while the authored metrics describe
 *   heavily enforced, suppressive operation with identifiable payers — the
 *   engine measures that divergence rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - state_enforcement_authorities: Agenda-setter and beneficiary seat (institutional/constrained) — enacts, interprets, and enforces the conception-anchored rule; acquires authority over pregnancy outcomes, medical records, and clinic operations
 *   - conceived_human_organisms: Declared beneficiary (powerless/trapped) — rights-bearer by stipulation; cannot act or litigate; interests administered entirely by others
 *   - prenatal_protection_advocacy_networks: Organized beneficiary (organized/mobile) — mission, standing, funding, and legislative pipeline flow from the rule; arbitrage-grade mobility across jurisdictions
 *   - pregnant_persons: Primary target (powerless/trapped) — bodily autonomy subordinated; criminal and civil exposure; individually isolated, collectively mounting ballot-box resistance
 *   - reproductive_healthcare_providers: Secondary target (moderate/constrained) — standard care carries felony exposure; licensure ties them to place
 *   - ivf_patients_and_providers: Collateral target (moderate/constrained) — routine embryo disposition becomes liability; services pause or relocate
 *   - medical_standard_of_care_bodies: Excluded expert voice (institutional/constrained) — clinical guidance overridden by statutory text
 *   - constitutional_bioethics_scholars: Analytical observer (analytical/analytical) — sees the full structure and its rival criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.82).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.85).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Conception-Anchored Legal Personhood Mandate").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '2e633542-6a81-4bec-8fbe-9627e661c4ef').
narrative_ontology:cs_kernel_codification('2e633542-6a81-4bec-8fbe-9627e661c4ef', formalized).
narrative_ontology:cs_authority_grounding('2e633542-6a81-4bec-8fbe-9627e661c4ef', lineage).
narrative_ontology:cs_interpretation_layer_present('2e633542-6a81-4bec-8fbe-9627e661c4ef').
narrative_ontology:cs_reading_relation('2e633542-6a81-4bec-8fbe-9627e661c4ef', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('2e633542-6a81-4bec-8fbe-9627e661c4ef', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('2e633542-6a81-4bec-8fbe-9627e661c4ef', foundational, fertilization_confers_intrinsic_full_status).
narrative_ontology:cs_axiom_status(fertilization_confers_intrinsic_full_status, holdable).
narrative_ontology:cs_axiom_grounding('2e633542-6a81-4bec-8fbe-9627e661c4ef', fertilization_confers_intrinsic_full_status, deontological).
narrative_ontology:cs_axiom('2e633542-6a81-4bec-8fbe-9627e661c4ef', foundational, developmental_potentiality_sufficiency).
narrative_ontology:cs_axiom_status(developmental_potentiality_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('2e633542-6a81-4bec-8fbe-9627e661c4ef', developmental_potentiality_sufficiency, deontological).
narrative_ontology:cs_reference_frame('2e633542-6a81-4bec-8fbe-9627e661c4ef', conception_anchored_full_personhood).
narrative_ontology:cs_drift_state('2e633542-6a81-4bec-8fbe-9627e661c4ef', contemporary_enforcement_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2e633542-6a81-4bec-8fbe-9627e661c4ef', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, conceived_human_organisms).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, prenatal_protection_advocacy_networks).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_authorities).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_providers).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, ivf_patients_and_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures enact conception-anchored personhood into statute and state constitution; attorneys general and district attorneys bring prosecutions; courts interpret and uphold. The arrangement extends their reach over medical decisions, pregnancy outcomes, and the data systems that record miscarriage and termination care. They administer the rule they wrote and cannot exit their own jurisdiction; their acquired authority persists only while the rule remains in force.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Declared rights-bearers from fertilization under this reading. They cannot act, communicate, or appear in proceedings; their interests are defined, argued, and settled entirely by state actors and advocacy organizations. Their situation ends either in birth, where the attached status converts into ordinary legal personality, or in pregnancy loss, where the status dissolves.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, conceived_human_organisms, beneficiary,
    powerless, biographical, trapped, local).

% Litigation shops, crisis-pregnancy center networks, model-legislation drafters, and aligned religious institutions. The reading supplies their core mission, standing to sue, fundraising basis, and legislative pipeline. They operate across state lines, choose their venues, and can redirect effort to whichever jurisdiction offers the next opening.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, prenatal_protection_advocacy_networks, beneficiary,
    organized, generational, mobile, continental).

% Carry pregnancies under rules they did not author and cannot veto. Where termination is barred, continuation is compelled; miscarriage management waits on legal review; pregnancy outcomes are discoverable in investigations. Crossing a state line remains possible but costly, sometimes monitored, and unavailable to those without money, documents, or time. Individually each faces the enforcement machinery alone; collectively they have begun reversing the rule at the ballot box where initiative procedure permits.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, trapped, regional).

% Physicians, midwives, nurses, and clinic staff whose ordinary standard of care now carries felony exposure in adopting jurisdictions. Licensure, hospital privileges, and malpractice coverage tie them to place; relocating means abandoning patients and practices built over decades. Some continue indicated care under legal risk and document defensively; some withhold it; some leave the state or the specialty.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_providers, payer,
    moderate, biographical, constrained, regional).

% Once fertilized embryos carry full legal status, routine clinic operations — cryopreservation, disposal of non-viable embryos, genetic screening, selective transfer — generate liability questions with no clinical answer. Clinics pause services, ship embryos out of state, or close; patients absorb delays, travel costs, or forfeit treatment altogether.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, ivf_patients_and_providers, payer,
    moderate, biographical, constrained, regional).

% Professional colleges and certifying boards whose clinical guidance on ectopic pregnancy, septic miscarriage, and lethal fetal anomaly is overridden by statutory text. They publish recommendations the adopting legislatures decline to incorporate; their expertise enters courtrooms as defense evidence rather than operating as the rule the system follows.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, medical_standard_of_care_bodies, excluded,
    institutional, generational, constrained, national).

% Map this reading against rival criteria for rights-attachment, trace its doctrinal consequences into contraception, IVF, and criminal law, testify in litigation, and document the divergence between the reading's stated premises and its operational effects.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_bioethics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_authorities).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__developmental_potentiality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the boundary question every legal system must answer — which entities bear rights — with a single bright line fixed at fertilization, giving courts, hospitals, prosecutors, and registries one uniform answer instead of case-by-case capacity assessment.
% TRANSFER_FUNCTION: Moves decisional authority over pregnancy, bodily autonomy, and criminal/civil liability exposure from pregnant persons and their providers to the state's enforcement apparatus, and confers a protected legal status (administered by state and advocacy institutions) on fertilized human organisms.
% ABSENT_VOICES: Pregnant persons appear as defendants and plaintiffs in enforcement cases but were absent from the rule's construction, which was authored by legislatures, courts, and advocacy organizations. Medical standard-of-care bodies are overridden rather than incorporated. The fertilized organism itself is spoken for exclusively by parties already committed to the reading. No seat inside the arrangement represents the option of leaving the boundary question to capacity-based assessment.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, regulation would revert to viability- or birth-anchored frameworks, pending prosecutions would collapse, IVF disposition practices would resume without liability review, interstate care networks built to route around the rule would stand down, and the advocacy litigation pipeline built on the reading would lose its object. Thousands of clinic, prosecutorial, and data-system arrangements reorganize around the removed rule.
% FOUNDING_PROBLEM: To prevent the termination of pregnancies by conferring full legal protection on the human organism from fertilization onward — consolidating, after decades in which capacity-based doctrine excluded fetal interests from constitutional protection, the movement's long-standing aim into enforceable positive law.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the anti-abortion movement and legal scholars across the dispute — including opponents of the reading — attest the genealogy and confirm the founding aim continues to be pursued wherever the rule is unenforced. What no source outside the benefiting parties attests is the moral premise itself: that fertilization confers full rights-bearing status. Corroboration covers the movement's continuity and sincerity, not the correctness of its criterion.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the arrangement compels continuation of pregnancy, exposes standard medical care to felony liability, and converts private medical decisions into prosecutable events — a taking of bodily autonomy and decisional authority from the governed class that no sibling reading imposes at this depth. Suppression is higher still (0.85) because persistence depends on active criminal enforcement, surveillance of pregnancy outcomes, and the legal foreclosure of rival criteria within adopting jurisdictions; the suppression is overwhelmingly structural (statutes, prosecutions, data systems) with a smaller internalized component flagged in an omega. Theater ratio is low-to-moderate (0.22): symbolic bills passed for enjoined effect and gestational-heartbeat framing carry ceremonial weight, but the bulk of enforcement activity is functionally real. Accessibility collapse is moderate (0.55): within adopting jurisdictions rival criteria collapse almost entirely, but interstate travel, out-of-state medication, and the continued national liveness of sibling readings keep alternatives partly reachable. Resistance is high (0.72): ballot measures have reversed the rule in several jurisdictions, providers practice civil disobedience, and care networks route around enforcement — the coalition-power question is whether that resistance has a ceiling, tracked in an omega. The measurement series run on one shared time grid ({0,10,20,30,40,52}) so every tracked metric is authored at every examined point; all three series rise monotonically, modeling an enforcement ratchet rather than a cycle — the reading spent roughly four decades as aspirational doctrine, then converted rapidly into enforced law, with extractiveness and suppression accumulating fastest in the final segment.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the state and advocacy seats the arrangement is protective coordination they built, fund, and staff — a rights extension, experienced as low-extraction. From the pregnant person's seat the same structure is compulsory gestation under threat of prosecution — maximal extraction with trapped exit. Providers sit between: professionally committed to the patients the rule overrides, legally bound by the rule they did not write. The engine derives these per-seat classifications from power, exit options, and directionality; the authored snare claim does not adjudicate among them, and the divergence between the agenda-setter's computed seat-type and the payer's is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Conceived human organisms sit nearest the beneficiary pole by declaration (d near 0.0) — the constraint exists to subsidize their declared status — though they are non-collecting beneficiaries whose interests are administered by others. Prenatal protection advocacy networks derive low d reinforced by arbitrage-grade exit: they gain mission, standing, and funding while bearing essentially none of the rule's burdens and can move between jurisdictions. State enforcement authorities derive low d as declared beneficiaries, with the caveat that their gain is authority itself rather than revenue. Pregnant persons derive the highest d in the story: declared victims, individually powerless, trapped by pregnancy, poverty, and jurisdiction. Reproductive healthcare providers and IVF patients/providers derive high d as victims with constrained exit — licensure, capital, and patient relationships bind them to place. National-to-continental scopes amplify effective extraction for the trapped seats because verification of compliance (and of miscarriage versus termination) grows harder as the enforcement surface widens. No directionality overrides were needed: the beneficiary/victim plus exit data produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem (preventing pregnancy termination, as the movement defines it) is live and actively pursued, and enforcement is intensifying rather than atrophying — this is the opposite of a piton profile, with theater_ratio low and rising only modestly. The classification work runs in the other direction: the reading presents itself as a rope-like extension of existing protection ('equal protection for the unborn'), and a naive classifier could accept that framing. Naming the victims (pregnant persons, providers, IVF participants), the enforcement dependence, and the suppressed alternatives is what prevents the coordination story from absorbing the extraction it rides on. Conversely, the analysis guards against overcorrection: the boundary question the kernel addresses is genuinely unavoidable — every legal system must draw it somewhere — so the extraction attributed to THIS reading is the margin above what any boundary rule would cost, not the entire institution of rights-attachment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (developmental_potentiality_reading) of the legal_personhood_boundary kernel; how would the sibling readings (functional_capacity_reading, restrictive_anthropocentric_reading) restructure the victim set and enforcement surface?',
    'Generate the sibling stories as separate files and compare computed per-seat classifications; the disagreement is located in the criterion for rights-attachment — trajectory-potentiality versus demonstrated capacity versus birth-plus-capacity.',
    'Adopting a sibling reading removes the embryo and fetus from the protected set, dissolves most of the state''s pregnancy-enforcement authority, and shrinks the victim set to whatever the rival criterion leaves unprotected; this story''s epsilon and classification do not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a contested personhood kernel, with structurally distinct siblings.').

omega_variable(
    moral_status_grounding,
    'Does developmental potentiality track a morally relevant property that grounds full rights-bearing status, or is the fertilization line a stipulated convention wearing natural-law dress?',
    'Cross-jurisdictional comparison of how the line performs against its own protective aims, plus convergence or persistent division in moral philosophy on potentiality arguments; no single empirical test settles a deontological premise, but operational failure modes are observable.',
    'If the line is stipulated, the arrangement''s protective justification weakens and its coercive operation stands unsupported by the coordination story it invokes; if grounded, part of the measured burden is the price of the protection the reading exists to deliver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_grounding, conceptual, 'Whether the reading''s foundational premise is a discovered moral fact or an imposed line.').

omega_variable(
    contraception_ivf_entailment,
    'Does the conception-anchored rule entail restrictions on contraceptives that may operate after fertilization and on routine IVF embryo disposition, or can enforcing authorities contain the rule''s application to termination?',
    'Observe enforcement patterns: prosecutions, clinic closures, and legislative extensions in adopting jurisdictions over the coming cycle.',
    'Broad entailment extends the victim set to contraception users and fertility patients at scale and deepens the suppression requirement; successful containment keeps the operative victim set narrower than the reading''s logic implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraception_ivf_entailment, empirical, 'Scope of the rule''s practical entailments beyond termination.').

omega_variable(
    suppression_mechanism_composition,
    'How much of the measured suppression on pregnant persons is structural (criminal exposure, surveillance, travel cost) versus internalized (shame norms, learned deference to legal authority over clinical judgment)?',
    'Post-repeal trajectory in jurisdictions that reverse the rule: if care-seeking and provider behavior normalize quickly, suppression was predominantly structural; if delay and self-surveillance persist, a substantial internalized component exists.',
    'An internalized component means the constraint''s effective suppression outlasts its formal repeal and should raise the persistence estimate for successor arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the suppression load.').

omega_variable(
    coalition_capacity_ceiling,
    'Can class-level coalition power — ballot initiatives, electoral turnover, jury nullification — dismantle the arrangement in jurisdictions where it is constitutionally entrenched, or does entrenchment set a hard ceiling on resistance?',
    'Track initiative outcomes and constitutional-amendment attempts against entrenchment depth over successive election cycles.',
    'If the ceiling holds, the arrangement persists despite high measured resistance and the snare classification stabilizes; if coalitions break through, affected jurisdictions transition toward repeal and the temporal series should show extractiveness falling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_capacity_ceiling, empirical, 'Whether dispersed victims can convert numbers into removal power against entrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 52).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(lega_tr_t52, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 52, 0.32).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(lega_be_t52, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 52, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(lega_su_t52, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 52, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, restrictive_anthropocentric_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'when does personhood begin' fails the epsilon-invariance test as a single constraint — measuring it by trajectory-potentiality yields a maximally extended protected set and a pregnancy-enforcement apparatus, while measuring it by demonstrated capacity yields a nearly empty fetal protected set and no pregnancy enforcement. These are different constraints with different epsilon values, victim sets, and failure modes, decomposed into three linked stories sharing the legal_personhood_boundary kernel. This story (developmental_potentiality_reading) is the most extractive member; the upstream kinship runs through shared doctrine — the siblings are cited and attacked by name in this reading's litigation and legislation, so edges run in both directions as mutual structural pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

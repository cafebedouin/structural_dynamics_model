% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Legal Personhood Boundary — Developmental Potentiality Reading (Birth-Line Regime Under Contest)
 *   domain: legal/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This story is ONE READING of the contested kernel
 *   legal_personhood_boundary: the developmental_potentiality_reading, which
 *   holds that rights-bearing status attaches at conception and that every
 *   holder of a human life trajectory is a rights-bearer. Per the fixed
 *   epsilon-referent rule for kernel-reading stories, epsilon is authored for
 *   the standing arrangement under contest — the birth-line personhood regime
 *   that withholds rights-bearing status from prenatal organisms and
 *   authorizes their destruction — as that arrangement is assessed by THIS
 *   reading's lights: from this seat the standing arrangement is
 *   near-maximally extractive against an entire class it refuses to count.
 *   The reading's endorsed alternative (the conception-line regime) is NOT
 *   the referent and is not described in the structural data; its shape —
 *   pregnant persons' autonomy subordinated to fetal rights, state
 *   enforcement authority over pregnancy outcomes — is recorded in the
 *   reading's axioms and the endorsed_counterfactual_structure omega. The
 *   sibling readings (functional_capacity_reading,
 *   restrictive_anthropocentric_reading) are separate constraint files over
 *   the same referent. Expected structural delta realized here: the fetus
 *   enters the victim set from conception (base_properties.victims) — the
 *   categorical marker distinguishing this reading from both siblings; the
 *   delta's remaining clauses describe the endorsed counterfactual and are
 *   quarantined accordingly.
 *
 * KEY AGENTS:
 *   - prenatal_human_organisms: primary target (powerless/trapped) — the class whose rights-bearing status the arrangement withholds from conception; bears the arrangement's full cost
 *   - pregnant_persons: primary beneficiary (moderate/constrained) — holds decision-authority over prenatal disposition under the arrangement's liberty line
 *   - reproductive_healthcare_providers: secondary beneficiary (organized/mobile) — holds the service franchise the boundary makes lawful
 *   - reproductive_rights_advocacy_institutions: beneficiary (institutional/identity_locked) — holds the doctrinal mandate; institutionally fused with the birth-line settlement
 *   - constitutional_courts: agenda_setter (institutional) — drew and defended the line; the 2022 reversal removed the federal defense
 *   - state_legislatures: agenda_setter (organized) — jurisdictional line-drawers; the contest's moving front since 2022
 *   - fetal_personhood_advocates: excluded (organized/constrained) — this reading's own mass movement, outside the authoritative doctrinal seat for the enforcement era
 *   - rights_theorists: analytical observer — no enforcement role, collects nothing, compares the three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.83).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.45).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.83).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Legal Personhood Boundary — Developmental Potentiality Reading (Birth-Line Regime Under Contest)").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '151d0105-5579-4d14-ba91-04a310bd58cc').
narrative_ontology:cs_kernel_codification('151d0105-5579-4d14-ba91-04a310bd58cc', formalized).
narrative_ontology:cs_authority_grounding('151d0105-5579-4d14-ba91-04a310bd58cc', lineage).
narrative_ontology:cs_interpretation_layer_present('151d0105-5579-4d14-ba91-04a310bd58cc').
narrative_ontology:cs_reading_relation('151d0105-5579-4d14-ba91-04a310bd58cc', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_reading_relation('151d0105-5579-4d14-ba91-04a310bd58cc', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_axiom('151d0105-5579-4d14-ba91-04a310bd58cc', foundational, personhood_begins_at_conception).
narrative_ontology:cs_axiom_status(personhood_begins_at_conception, holdable).
narrative_ontology:cs_axiom_grounding('151d0105-5579-4d14-ba91-04a310bd58cc', personhood_begins_at_conception, deontological).
narrative_ontology:cs_axiom('151d0105-5579-4d14-ba91-04a310bd58cc', foundational, developmental_trajectory_suffices_for_status).
narrative_ontology:cs_axiom_status(developmental_trajectory_suffices_for_status, holdable).
narrative_ontology:cs_axiom_grounding('151d0105-5579-4d14-ba91-04a310bd58cc', developmental_trajectory_suffices_for_status, deontological).
narrative_ontology:cs_reference_frame('151d0105-5579-4d14-ba91-04a310bd58cc', conception_threshold_personhood).
narrative_ontology:cs_drift_state('151d0105-5579-4d14-ba91-04a310bd58cc', contemporary_post_dobbs_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('151d0105-5579-4d14-ba91-04a310bd58cc', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_providers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, reproductive_rights_advocacy_institutions).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, prenatal_human_organisms).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, birth_threshold_personhood_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, substantive_due_process_autonomy_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Human organisms in utero from conception onward. Under the birth-line arrangement they hold no rights-bearing status: no standing to assert interests in any court, no legal identity before birth, no protection against destruction, which the arrangement authorizes. They receive nothing from the arrangement and bear its entire cost. Exit does not exist for this seat: the class cannot migrate, consent, or opt out, and the boundary fixes their status from conception until birth. Where jurisdictions have enacted fetal-personhood provisions, members of this class gain partial protection; that patchwork is the contest's front line.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, prenatal_human_organisms, payer,
    powerless, biographical, trapped, global).

% Carry pregnancies and hold decision-authority over continuation or termination; the arrangement's liberty line shields that decision from state override in permissive jurisdictions. What flows to them is bodily liberty and decision-authority; what flows from them is the exercise of that authority over the prenatal organism's disposition. Exit is jurisdictional and unevenly distributed by income: access varies sharply across states and countries. The developmental-potentiality reading asserts their autonomy claims are subordinate to fetal rights from conception; under that reading's operative line their decision-authority would be subordinated and, in enforcement regimes, subject to state oversight of pregnancy outcomes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, global).

% Deliver termination and prenatal care; the lawful core of that practice depends on the boundary — prenatal organisms are not patients with rights. What flows to them is the service franchise and its revenue; under the conception line their central service would be unlawful and, in enforcement regimes, criminally exposed. Exit is professional mobility across jurisdictions, uneven by subspecialty and by the legal exposure attached to the practice.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_providers, beneficiary,
    organized, biographical, mobile, global).

% Litigate, legislate, and fund the defense of the birth-line settlement. What flows to them is mandate, funding, standing in the doctrinal conversation, and organizational continuity. Their institutional identity is constituted by the fight: the organizations became their function across five decades of litigation, so exit would mean mission dissolution rather than repositioning.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, reproductive_rights_advocacy_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Drew and defended the personhood line: defined rights-bearing status, adjudicated conception-personhood enactments and wrongful-death theories, and for five decades struck state restrictions that breached the federal line. The 2022 reversal removed the federal defense and devolved line-drawing to the states. What flows to them is docket control over the most contested boundary in law; exit is bounded by precedent and jurisdiction — they cannot leave the contest, only re-rule it.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Enact the line jurisdiction by jurisdiction: personhood statutes, abortion regulation, fetal-homicide provisions, wrongful-death theories. Since 2022 they are the primary line-drawers; some have moved toward the conception threshold through trigger statutes and personhood amendments, others have codified the birth-line with statutory protection. What flows to them is agenda control over pregnancy law within their borders; exit is none — the contest is their institutional terrain.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_legislatures, agenda_setter,
    organized, biographical, constrained, national).

% A mass movement asserting personhood from conception: legislation, constitutional amendments, wrongful-death litigation, crisis-pregnancy infrastructure, doctrinal scholarship. Excluded from the authoritative doctrinal seat for the arrangement's enforcement era — enactments struck down, positions recorded but not adopted — they could not exit the dispute: the boundary binds them wherever they live. Since 2022 their exclusion is partial and jurisdictional; they hold legislative power in a subset of jurisdictions and none in others.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetal_personhood_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the boundary across the readings — the metaphysics of moral status, the administrability of thresholds, the doctrinal genealogy of the birth line. Hold no enforcement role, collect nothing from the arrangement, and cannot be bound by it; their seat is where the three readings of the kernel are compared.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, rights_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__developmental_potentiality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The birth-line solves the legal-status assignment problem: one observable, administrable event (live birth) that every registry, court, and statute can apply without metaphysical inquiry into prenatal development. It coordinates homicide law, inheritance, tort, and constitutional analysis on a single threshold.
% TRANSFER_FUNCTION: Moves legal status and decision-authority: withholds rights-bearing status from prenatal organisms from conception to birth and confers decision-authority over prenatal disposition on pregnant persons, with the service franchise to providers and the doctrinal mandate to advocacy institutions.
% ABSENT_VOICES: The prenatal organisms themselves — the class whose standing is the object of the contest — cannot appear and are represented only through advocates. Fetal-personhood advocates were structurally excluded from the authoritative doctrinal conversation for the arrangement's enforcement era (enactments struck, positions unheard); since 2022 their exclusion is partial and jurisdictional. The strongest excluded voice is the party that cannot speak at all.
% DISAPPEARANCE_RATIONALE: If the birth-line arrangement vanished overnight, every jurisdiction would need an operative personhood rule: homicide, inheritance, tort, IVF regulation, and obstetric practice all presuppose one. The reproductive-medical-legal complex would reorganize around whatever line replaced it, and the contest's parties would immediately re-fight the replacement. Nothing in the arrangement's infrastructure is self-maintaining.
% FOUNDING_PROBLEM: The arrangement was built to solve the collision between pregnant women's liberty and equality claims and the legal significance of prenatal life, under the administrability constraint that any line short of birth invites unresolvable metaphysical and evidentiary litigation. The settlement located rights-bearing at birth and protected termination as a liberty interest, trading metaphysical contestability for a bright line.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: natural-law jurists and prenatal-rights scholars attest the collision was real but mis-resolved; medical-historical scholarship corroborates the pre-legalization mortality problem the settlement addressed; the sibling readings attest the administrability problem was genuine while disputing the birth threshold. No serious party denies the collision existed — what is contested is whether the birth-line was a solution or a dodge.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.83, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.83 from the reading's seat: the arrangement withholds all standing from an entire class this reading holds to be rights-bearing and authorizes its destruction at scale, which is near-maximal extraction against the class it governs; the modest post-2022 decline reflects jurisdictional restrictions and fetal-personhood gains biting at the margin. Suppression (0.45 at end-state) is a raw structural property, unscaled by power or scope: the arrangement's enforcement ratchet peaked at 0.80 (courts striking enactments, doctrine thickening to hold the line against a mass counter-movement) and collapsed at the 2022 rupture when the federal enforcement arm withdrew — the series is authored because the story specifically traces enforcement-capacity change. Theater (0.62) reflects a doctrinal apparatus — viability lines, undue-burden balancing, trimester frameworks — that from this seat performed principled boundary-drawing while the operative function ran beneath it; the 2022 collapse exposed much of the apparatus as performance, with partial re-functionalization as state lines stabilize. Accessibility_collapse is low (0.40): the alternative never collapsed — the conception-line position persisted as mass movement, legislation, and litigation across the whole interval, and post-2022 it holds partial jurisdictional power. Resistance is high (0.80): one of the largest sustained resistance campaigns in modern constitutional politics. Claim/metric independence: claimed_type snare is this reading's structural assessment of the standing arrangement (coordination-as-cover, identifiable victims, suppressed alternative); the metrics are authored descriptively of the arrangement's operation as this reading assesses it — neither was tuned to the other or to any predicted engine output. Coalition note: the victim class cannot coordinate at all — it has no capacity for action — so its coalition power runs entirely through its excluded advocates, which is why its exit is trapped rather than merely constrained.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from the same structural data. From the prenatal-organism seat — constructed by this reading as a rights-bearing party with trapped exit — the arrangement computes as pure extraction with no offsetting coordination benefit. From the pregnant-person seat the same arrangement computes as liberty-protecting coordination whose costs fall elsewhere entirely. From the courts' seat it computes as administrable doctrine: a bright line solving a real legal-status assignment problem. The engine computes these per-seat classifications from the declared roles, power, and exit options; the divergence between the trapped payer seat and the beneficiary seats is the measurement this story exists to take, and no authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (pregnant_persons, reproductive_healthcare_providers, reproductive_rights_advocacy_institutions) derive low directionality — the arrangement subsidizes these seats with decision-authority, service franchise, and doctrinal mandate, so effective extraction damps or inverts for them. The victim declaration (prenatal_human_organisms) derives near-full-target directionality, amplified by trapped exit: the class cannot migrate, consent, or opt out, and the boundary fixes its status from conception. The agenda_setter seats (courts, legislatures) derive from their enforcement role. No directionality overrides are used: the beneficiary/victim structure plus exit options produce the correct d values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling pregnant women's liberty and equality claims with the legal significance of prenatal life under an administrability constraint — is authored as contested: this reading holds the reconciliation was misconceived (no legitimate settlement excludes rights-bearers), while the arrangement's defenders hold the collision is permanent. The mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): no dead-mandate flag fires, correctly — the collision the arrangement manages is live, and every jurisdiction needs an operative line. The mandatrophy discipline also blocks the inverse error: mislabeling the reading's own endorsed arrangement as pure coordination. The conception line, if operative, would carry its own costs (autonomy subordination, pregnancy-conduct enforcement, IVF embryo-disposal restrictions) — which is why the counterfactual is quarantined in an omega and flagged for its own story rather than folded into this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_declaration,
    'This constraint is one reading of the legal_personhood_boundary kernel — the developmental_potentiality_reading. What do the sibling readings (restrictive_anthropocentric_reading, functional_capacity_reading) instantiate structurally differently over the same kernel?',
    'Comparative authoring of the sibling stories: each names its own victim set, epsilon, and axioms over the shared referent; cross-reading comparison locates the disagreement in boundary placement, not in the extraction arithmetic.',
    'If a sibling reading were adopted as operative law, the victim set changes categorically: the prenatal class exits this story''s victim set only by leaving this reading; the restrictive_anthropocentric_reading never admits it; the functional_capacity_reading substitutes a capacity test for both species-membership and developmental stage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_declaration, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling readings are separate constraint files.').

omega_variable(
    shared_referent_reading_indexed_epsilon,
    'Epsilon is authored for the shared referent (the standing birth-line personhood arrangement) indexed to this reading''s own lights, per the kernel-reading referent rule; the sibling readings author different epsilon over the same referent. Is the referent stable across the interval given the 2022 enforcement rupture?',
    'Define the referent''s identity condition by the threshold rather than the enforcing institution: a jurisdiction instantiates the referent while it withholds rights-bearing status from prenatal organisms, regardless of whether a constitutional court, a legislature, or no institution actively enforces the withholding.',
    'If the referent were instead defined by enforcement institution (federal constitutional doctrine), the constraint would split at 2022 into two stories with different epsilon trajectories; the shared-threshold definition keeps one epsilon-invariant constraint across the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_referent_reading_indexed_epsilon, conceptual, 'Reading-indexed epsilon over a shared referent; referent identity condition declared.').

omega_variable(
    endorsed_counterfactual_structure,
    'If this reading''s conception threshold were adopted as the operative boundary, what structure would the resulting arrangement carry — and is that structure described anywhere in this story''s structural data?',
    'It is not: this story''s structural data describes the standing arrangement (the epsilon referent), never the reading''s endorsed alternative. The counterfactual — pregnant persons'' autonomy subordinated to fetal rights, state enforcement authority over pregnancy outcomes, prenatal organisms as rights-bearers — is recorded in the reading''s axioms and here, and warrants its own sibling story.',
    'Prevents conflation of the reading''s assessment seat with its endorsed arrangement. The corpus should carry the counterfactual arrangement as a separate constraint with its own epsilon; note that this reading would author that story''s epsilon near zero, which is exactly the advocacy-reading failure the referent rule exists to block on THIS story''s referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endorsed_counterfactual_structure, conceptual, 'Quarantines the reading''s endorsed arrangement outside the structural data; flags it as a needed separate story.').

omega_variable(
    post_dobbs_enforcement_fragmentation,
    'After the 2022 rupture, is the birth-line arrangement one constraint or a jurisdictional family — permissive jurisdictions still excluding conception-personhood claims, restrictive jurisdictions enforcing toward (and in some cases past) this reading''s line, including pregnancy-conduct oversight?',
    'Track whether jurisdictional personhood lines converge or diverge over the following decade: convergence on a new operative threshold restores a single constraint; durable divergence splits the family into per-jurisdiction stories.',
    'Durable divergence would date this story''s referent to the pre-2022 federal regime and require per-jurisdiction decomposition; convergence preserves the single-referent framing and the authored epsilon trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_dobbs_enforcement_fragmentation, empirical, 'Referent-fragmentation risk after the 2022 enforcement collapse.').

omega_variable(
    metaphysical_status_unresolvability,
    'This reading''s foundational axiom (personhood from conception) is deontological and not empirically falsifiable, while the functional_capacity sibling grounds in empirically contingent capacity claims. Is any data set capable of adjudicating the kernel, or is the contest irreducibly normative?',
    'No empirical resolution exists for the deontological axiom; the contest resolves only through conventional settlement — courts, amendments, constitutional revision. Track institutional settlement, not discovery, as the only resolution channel.',
    'Classification of every reading in this kernel is stable under evidence but unstable under institutional settlement; drift in this kernel tracks authority shifts rather than empirical findings, so temporal analysis should weight the suppression series over any expectation of evidentiary resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_status_unresolvability, conceptual, 'The kernel''s contest is normative, not empirical; resolution runs through settlement only.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 53).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_boundary_dpr_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(personhood_boundary_dpr_tr_t10, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(personhood_boundary_dpr_tr_t19, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 19, 0.48).
narrative_ontology:measurement(personhood_boundary_dpr_tr_t27, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 27, 0.52).
narrative_ontology:measurement(personhood_boundary_dpr_tr_t35, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 35, 0.55).
narrative_ontology:measurement(personhood_boundary_dpr_tr_t44, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 44, 0.58).
narrative_ontology:measurement(personhood_boundary_dpr_tr_t49, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 49, 0.66).
narrative_ontology:measurement(personhood_boundary_dpr_tr_t53, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 53, 0.62).

% Extraction over time
narrative_ontology:measurement(personhood_boundary_dpr_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement(personhood_boundary_dpr_be_t10, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(personhood_boundary_dpr_be_t19, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 19, 0.86).
narrative_ontology:measurement(personhood_boundary_dpr_be_t27, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 27, 0.87).
narrative_ontology:measurement(personhood_boundary_dpr_be_t35, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 35, 0.87).
narrative_ontology:measurement(personhood_boundary_dpr_be_t44, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 44, 0.86).
narrative_ontology:measurement(personhood_boundary_dpr_be_t49, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 49, 0.84).
narrative_ontology:measurement(personhood_boundary_dpr_be_t53, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 53, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(personhood_boundary_dpr_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(personhood_boundary_dpr_su_t10, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(personhood_boundary_dpr_su_t19, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 19, 0.7).
narrative_ontology:measurement(personhood_boundary_dpr_su_t27, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 27, 0.74).
narrative_ontology:measurement(personhood_boundary_dpr_su_t35, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(personhood_boundary_dpr_su_t44, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 44, 0.8).
narrative_ontology:measurement(personhood_boundary_dpr_su_t49, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 49, 0.45).
narrative_ontology:measurement(personhood_boundary_dpr_su_t53, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 53, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the legal_personhood_boundary kernel decomposes into three reading-constraints — developmental_potentiality_reading (this story), functional_capacity_reading, and restrictive_anthropocentric_reading. All three share one referent (the standing birth-line personhood arrangement) and author different epsilon over it per reading-indexed valuation; their victim sets differ categorically (prenatal class / sentient non-humans / none), and their boundary placements are mutually exclusive within any single legal framework. The standing birth-line doctrine is the upstream arrangement all three assess; no member of the family is authoritative over the others — the contest is the structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

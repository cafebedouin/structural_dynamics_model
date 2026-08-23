% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living-Constitution Reading of the U.S. Constitution (Judicially Administered Evolving-Meaning Regime)
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   Under the living reading, the Constitution of 1787 functions as an
 *   aspirational framework whose meaning is legitimately revised by each
 *   generation's courts in light of evolving societal norms — privacy,
 *   dignity, equal standing — rather than frozen at ratification-era
 *   understanding. This story instantiates that single reading as one
 *   epsilon-invariant constraint: the standing arrangement under contest is
 *   the judicially administered evolving-meaning regime itself, and epsilon
 *   is assessed by the reading's own lights — the regime as the living reader
 *   sees it: adaptive and protective, but increasingly captured by the
 *   professional class that operates it. The arrangement carries a genuine
 *   coordination function (a fixed eighteenth-century text remains operable
 *   across transformation without perpetual supermajority amendment)
 *   alongside an asymmetric transfer of policy-adjudication authority from
 *   elected bodies to courts and the legal academy. KEY AGENTS (by structural
 *   relationship): federal_judiciary — agenda-setting interpreter
 *   (institutional/identity_locked) administering the regime and collecting
 *   interpretive supremacy; elite_legal_academy — primary beneficiary
 *   (organized/mobile) supplying frameworks and credentialing interpreters;
 *   rights_claimant_movements — secondary beneficiary (moderate/constrained)
 *   receiving protections via litigation; state_and_local_majorities —
 *   primary target (powerful/trapped) losing enacted policy to preemption;
 *   originalist_legal_movement — opposing target (organized/constrained)
 *   whose interpretive project loses ground; ordinary_citizens —
 *   dual-positioned (powerless/trapped), gaining protections while losing
 *   policy voice; political_theory_community — analytical observer seeing the
 *   full structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.72).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.75).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living-Constitution Reading of the U.S. Constitution (Judicially Administered Evolving-Meaning Regime)").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '4560512d-c81a-4791-8c5d-dc58c9c227a9').
narrative_ontology:cs_kernel_codification('4560512d-c81a-4791-8c5d-dc58c9c227a9', fixed_text).
narrative_ontology:cs_authority_grounding('4560512d-c81a-4791-8c5d-dc58c9c227a9', lineage).
narrative_ontology:cs_interpretation_layer_present('4560512d-c81a-4791-8c5d-dc58c9c227a9').
narrative_ontology:cs_reading_relation('4560512d-c81a-4791-8c5d-dc58c9c227a9', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4560512d-c81a-4791-8c5d-dc58c9c227a9', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('4560512d-c81a-4791-8c5d-dc58c9c227a9', foundational, text_embodies_transhistorical_principles).
narrative_ontology:cs_axiom_status(text_embodies_transhistorical_principles, holdable).
narrative_ontology:cs_axiom_grounding('4560512d-c81a-4791-8c5d-dc58c9c227a9', text_embodies_transhistorical_principles, deontological).
narrative_ontology:cs_axiom('4560512d-c81a-4791-8c5d-dc58c9c227a9', secondary, evolving_norms_license_meaning_revision).
narrative_ontology:cs_axiom_status(evolving_norms_license_meaning_revision, holdable).
narrative_ontology:cs_axiom_grounding('4560512d-c81a-4791-8c5d-dc58c9c227a9', evolving_norms_license_meaning_revision, instrumental).
narrative_ontology:cs_reference_frame('4560512d-c81a-4791-8c5d-dc58c9c227a9', aspirational_framework_under_judicial_stewardship).
narrative_ontology:cs_drift_state('4560512d-c81a-4791-8c5d-dc58c9c227a9', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4560512d-c81a-4791-8c5d-dc58c9c227a9', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, elite_legal_academy).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, rights_claimant_movements).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_and_local_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_legal_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, ordinary_citizens).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, ordinary_citizens).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, living_tree_interpretation_principle).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, substantive_due_process_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, evolving_standards_of_decency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides what the Constitution requires, case by case, and each cohort of appointees reshapes the body of doctrine it inherits. Justifies updated meanings as fidelity to the text's enduring principles. Collects final say over contested social questions. Members face confirmation battles and legitimacy criticism but cannot step outside the office: adjudicating meaning is what the office is, so departure from the role is not available as an option.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, federal_judiciary, beneficiary).

% Law faculties, elite schools, and the professional journal ecosystem supply the interpretive theories, train the appointees, and credential what counts as serious argument. Collects prestige, citation networks, advisory posts, and relevance. Movement between academy, government, and practice is routine, so shifting intellectual frameworks costs individual members little.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, elite_legal_academy, beneficiary,
    organized, generational, mobile, national).

% Organized groups seeking protection for privacy, bodily autonomy, equal standing, and dignitary interests pursue recognition through litigation because electoral and amendment routes are slow or blocked. Gains arrive as judicial doctrines. Their access depends on continued favorable interpretation, so they defend the arrangement that protects them.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, rights_claimant_movements, beneficiary,
    moderate, biographical, constrained, national).

% Elected bodies enact policy reflecting their constituents' preferences; courts set aside enactments that conflict with updated doctrine. They cannot leave the constitutional order, and amending it requires supermajorities no single state bloc can muster. They bear the loss of policy space and litigate defensively at recurring cost.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_and_local_majorities, payer,
    powerful, biographical, trapped, regional).

% Scholars, advocacy organizations, and aligned judges committed to ratification-era meaning invest careers in historical research that updated doctrine repeatedly rules beside the point. They can influence appointments but cannot exit the shared constitutional order whose meaning they contest, so they fight inside it indefinitely.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_legal_movement, payer,
    organized, generational, constrained, national).

% Receive expanded protections — privacy, dignity, equal treatment — that no legislature gave them and no ballot asked them about. At the same time, disputes they might have settled through elections are decided in courtrooms instead. Citizenship is not practically renounceable, and their experience of the arrangement depends on whether the current doctrine favors their side.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, ordinary_citizens, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, ordinary_citizens, payer).

% Theorists of democracy, legitimacy, and constitutionalism analyze whether judicial updating sustains or corrodes self-government, publishing critiques from both directions. Holds no stake in any doctrine's survival and sees the whole structure from outside it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, political_theory_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single fixed eighteenth-century supreme text operable and authoritative across radical technological and social change, providing continuity and one shared governing framework where perpetual formal amendment would be impracticable.
% TRANSFER_FUNCTION: Moves final policy-adjudication authority from elected majorities and state governments to federal courts and the legal-professional class that supplies their interpretive frameworks; simultaneously moves rights protections toward groups that lack electoral leverage.
% ABSENT_VOICES: State legislators whose enactments are set aside appear only as losing litigants after the fact; ordinary citizens never consented to judicial norm-updating and hold no seat in doctrine formation; original-meaning adherents hold loud public seats but are structurally outvoted inside the interpretive institution itself. Apparent consensus about the arrangement's legitimacy exists mainly among the seats the arrangement empowers.
% DISAPPEARANCE_RATIONALE: If the evolving-meaning regime vanished overnight, doctrines founded on updated interpretation — substantive privacy and bodily-autonomy protections, expansive equal-standing guarantees, evolving-standards reasoning in punishment — lose their foundation; large bodies of precedent become re-arguable on text plus history; states move quickly to re-enact long-set-aside statutes; and the federal judiciary's policy role contracts sharply. The constitutional order rearranges around whichever rival account of the text captures the vacuum.
% FOUNDING_PROBLEM: A supreme law drafted under eighteenth-century conditions — limited franchise, agrarian economy, no administrative state, no recognized privacy or dignitary rights — must govern a continental industrial democracy. The living reading was built to solve the obsolescence problem: keeping the fixed text authoritative and protective without requiring perpetual Article V supermajority amendment.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians document the 1787-text-versus-modern-society mismatch independently of any interpretive camp; comparative constitutionalists corroborate that national constitutions rarely survive two generations without adaptation mechanisms; and originalist scholars themselves — adversaries of this reading — concede the amendment-rigidity problem while disputing the judicial remedy. Corroboration therefore exists outside the beneficiary set, though every corroborating seat disputes this reading's particular solution.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon = 0.72) reflects the scale of policy space moved from electoral to judicial control across the interval, discounted by the real protections delivered to groups lacking electoral leverage. Suppression (0.75) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. It measures the enforcement machinery the reading requires: appointment politics, precedent hierarchy, professional gatekeeping, and the marginalization of original-meaning argument within elite venues. Suppression here is predominantly structural with an internalized component (legal-professional socialization that renders historical-evidence argument presumptively unserious); the split is carried in the elite_capture omega. Theater ratio (0.40) captures the growing share of 'evolving norms' rhetoric that launders professional preference as social consensus — functional adaptation remains the majority activity, but performative invocation rises as the doctrine ages. Accessibility collapse is low (0.35): rival accounts of the text remain fully live, which is precisely why enforcement effort stays high. Resistance (0.68) is correspondingly strong: sustained political movements, appointment conflicts, and periodic doctrinal reversals. The temporal series runs on one shared grid (1937-2027, six points, every tracked metric at every point); trajectories show a ratchet rather than a cycle — each governing coalition, once it captures the interpretive machine, exercises it, so net concentration of policy authority rises monotonically even as particular doctrines reverse. Boltzmann coordination type is enforcement_mechanism: the coordination function is maintaining one authoritative legal framework across time, which requires dedicated enforcement infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the federal_judiciary seat the arrangement is the institution's own stewardship — adaptive interpretation it performs and benefits from; identity fusion with the interpretive role (the Court has become its function) makes the seat experience the regime as constitutive rather than imposed, and breaking the professional-judicial identity frame would convert the seat's experience toward imposed burden. From the elite_legal_academy seat it is a prestige-and-relevance engine with cheap exit. From rights_claimant_movements it is the only viable protection channel. From state_and_local_majorities and the originalist_legal_movement the same structure operates as dispossession — enacted policy set aside, historical evidence ruled immaterial. Ordinary citizens sit near-symmetric: protections received without consent, policy voice surrendered without a vote. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (federal_judiciary, elite_legal_academy, rights_claimant_movements) drive d toward the beneficiary end for those seats — amplified for the academy by arbitrage-grade exit (mobile), and pushed toward deep capture for the judiciary by identity lock, which binds the seat to the arrangement it administers. Victim declarations (state_and_local_majorities, originalist_legal_movement) drive d toward the full-target end, amplified by trapped and constrained exit: neither can leave the constitutional order, and Article V supermajority requirements make exit-through-amendment effectively unavailable. Ordinary citizens carry a dual position at the stakeholder level and derive near-symmetric treatment. National spatial scope raises verification difficulty, modestly amplifying effective extraction for target seats. No directionality overrides are authored: the derivation from declarations plus exit options reproduces the intended relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an eighteenth-century supreme text governing a transformed society — remains live, so mandatrophy is not resolved and none is declared. The tangled_rope classification prevents two symmetrical mislabels: reading the arrangement as pure rope would erase the documented transfer of policy authority to an insulated professional class (the elite-capture vulnerability this reading is known to carry); reading it as pure snare would erase the genuine coordination achievement — continuity and rights incorporation that neither the amendment rate nor fixed-meaning adjudication has replicated. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no dead-mandate flag fires, correctly, because the problem the arrangement manages has not gone away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_capture_of_evolving_norms,
    'Are the ''evolving norms'' this reading incorporates genuinely societal consensuses, or professional-elite preferences laundered as social consensus?',
    'Divergence analysis between judicial norm-attribution and persistent mass opinion: track cases where courts attribute change to ''evolving standards'' against polling and legislative records showing durable pluralities the other way.',
    'If captured, effective extraction rises above the authored epsilon and the arrangement drifts snare-ward — coordination cover for professional-class advantage; if genuinely societal, the coordination share is larger and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Whether incorporated ''norms'' are social consensuses or professional-elite preferences.').

omega_variable(
    kernel_reading_commitment,
    'Which structural features of this constraint belong to the living reading specifically rather than to the us_constitution_1787 kernel, and what would sibling readings change?',
    'Compare against the sibling stories (originalist_reading, positivist_reading): the disagreement is located in who holds authority to revise constitutional meaning — courts reading evolving norms (this reading), ratification-era fixity binding later interpreters (originalist), or the Article V amendment process exclusively (positivist).',
    'Sibling readings author different epsilon over the same referent: the originalist reading sees the same standing arrangement as usurpation (higher epsilon, snare-ward); the positivist reading sees it as extra-textual lawmaking concentrated on the judicial-discretion component. This file''s epsilon is reading-indexed, not topic-invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; epsilon is reading-indexed over a fixed referent.').

omega_variable(
    adaptation_authority_separability,
    'Is adaptive constitutional interpretation separable from judicial monopoly over it — could the coordination function (keeping a fixed text operable) persist under dispersed adaptation authority such as simplified amendment, popular constitutionalism, or departmental review?',
    'Comparative constitutional analysis of jurisdictions with easier formal amendment or multiple authoritative interpreters: does adaptability survive without concentrated judicial updating?',
    'If separable, a large share of measured extraction is monopoly advantage riding on a real coordination function; if inseparable, much of the extraction is the price of the adaptation itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_authority_separability, conceptual, 'Whether the coordination and concentration components of the arrangement are structurally separable.').

omega_variable(
    democratic_displacement_magnitude,
    'How much policy space has actually been displaced from electoral control by evolving-meaning adjudication, net of invalidations that would have occurred under any interpretive regime?',
    'Counterfactual coding of set-aside enactments: would original-meaning or positivist adjudication have reached the same outcomes? Difference-in-differences across doctrinal regimes.',
    'If displacement is largely regime-specific, the authored epsilon is well-placed; if most invalidations would occur under any reading, the living reading''s incremental extraction is smaller than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_displacement_magnitude, empirical, 'Net magnitude of policy-space displacement attributable to this reading specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1937, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__living_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t1937, observed).
narrative_ontology:measurement(us_c_tr_t1957, us_constitution_1787__living_reading, theater_ratio, 1957, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t1957, observed).
narrative_ontology:measurement(us_c_tr_t1977, us_constitution_1787__living_reading, theater_ratio, 1977, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t1977, observed).
narrative_ontology:measurement(us_c_tr_t1997, us_constitution_1787__living_reading, theater_ratio, 1997, 0.3).
narrative_ontology:measurement_basis(us_c_tr_t1997, observed).
narrative_ontology:measurement(us_c_tr_t2017, us_constitution_1787__living_reading, theater_ratio, 2017, 0.36).
narrative_ontology:measurement_basis(us_c_tr_t2017, observed).
narrative_ontology:measurement(us_c_tr_t2027, us_constitution_1787__living_reading, theater_ratio, 2027, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t2027, projected).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__living_reading, base_extractiveness, 1937, 0.38).
narrative_ontology:measurement_basis(us_c_be_t1937, observed).
narrative_ontology:measurement(us_c_be_t1957, us_constitution_1787__living_reading, base_extractiveness, 1957, 0.48).
narrative_ontology:measurement_basis(us_c_be_t1957, observed).
narrative_ontology:measurement(us_c_be_t1977, us_constitution_1787__living_reading, base_extractiveness, 1977, 0.6).
narrative_ontology:measurement_basis(us_c_be_t1977, observed).
narrative_ontology:measurement(us_c_be_t1997, us_constitution_1787__living_reading, base_extractiveness, 1997, 0.66).
narrative_ontology:measurement_basis(us_c_be_t1997, observed).
narrative_ontology:measurement(us_c_be_t2017, us_constitution_1787__living_reading, base_extractiveness, 2017, 0.7).
narrative_ontology:measurement_basis(us_c_be_t2017, observed).
narrative_ontology:measurement(us_c_be_t2027, us_constitution_1787__living_reading, base_extractiveness, 2027, 0.72).
narrative_ontology:measurement_basis(us_c_be_t2027, projected).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__living_reading, suppression_requirement, 1937, 0.42).
narrative_ontology:measurement_basis(us_c_su_t1937, observed).
narrative_ontology:measurement(us_c_su_t1957, us_constitution_1787__living_reading, suppression_requirement, 1957, 0.5).
narrative_ontology:measurement_basis(us_c_su_t1957, observed).
narrative_ontology:measurement(us_c_su_t1977, us_constitution_1787__living_reading, suppression_requirement, 1977, 0.58).
narrative_ontology:measurement_basis(us_c_su_t1977, observed).
narrative_ontology:measurement(us_c_su_t1997, us_constitution_1787__living_reading, suppression_requirement, 1997, 0.66).
narrative_ontology:measurement_basis(us_c_su_t1997, observed).
narrative_ontology:measurement(us_c_su_t2017, us_constitution_1787__living_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement_basis(us_c_su_t2017, observed).
narrative_ontology:measurement(us_c_su_t2027, us_constitution_1787__living_reading, suppression_requirement, 2027, 0.75).
narrative_ontology:measurement_basis(us_c_su_t2027, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the U.S. Constitution' conflates three structurally distinct constraints — three readings of one kernel (us_constitution_1787). Each reading gets its own epsilon, its own beneficiary/victim structure, and its own classification; this file authors the living_reading only. The upstream sibling (originalist_reading, higher empirical anchoring in ratified text and history) and the procedural sibling (positivist_reading) are linked here because each is routinely cited as the corrective to this reading's excesses and vice versa; contamination propagates across the family through appointment politics and doctrinal reversal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

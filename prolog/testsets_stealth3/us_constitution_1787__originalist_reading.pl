% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Reading: Constitutional Meaning Fixed at Ratification
 *   domain: legal/constitutional/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the United States
 *   Constitution as a standing interpretive arrangement: constitutional
 *   meaning is fixed at ratification, the framers' and ratifiers' intent
 *   binds later interpreters, and legitimate constitutional change travels
 *   only through Article V. The arrangement coordinates every interpreter of
 *   the fundamental law against a single stable reference point — the genuine
 *   collective-action service — while the same boundary that stabilizes also
 *   allocates: claims framed in eighteenth-century categories pass; claims to
 *   healthcare, housing, digital privacy, and environmental protection die at
 *   the threshold; pre-1789 practices retain legitimacy; and the authority to
 *   say what the founding meant concentrates in a credentialed cadre holding
 *   adjudicative power. Enforcement is active and infrastructural: aligned
 *   judicial appointments, law-school pipelines, a dedicated scholarly
 *   apparatus supplying usable founding-era evidence, and an amicus industry.
 *   The claim/metric independence rule applies: claimed_type is authored from
 *   structural analysis (both a coordination function and asymmetric
 *   allocation are present, actively enforced), while the metrics describe
 *   the arrangement's observed operation over its modern arc (interval 0-50
 *   maps approximately to 1970-2020). Sibling readings — living and
 *   positivist — are separate constraints with their own epsilon, linked
 *   through network.affects_constraints; this file authors only the
 *   fixed-at-ratification arrangement. KEY AGENTS (by structural
 *   relationship): - originalist_legal_establishment: agenda-setter and
 *   primary beneficiary (institutional/identity_locked) — administers the
 *   fixed-meaning method and collects interpretive authority -
 *   states_rights_advocates: beneficiary (organized/constrained) -
 *   property_rights_coalitions: beneficiary (powerful/arbitrage) -
 *   modern_social_rights_claimants: primary target (organized/trapped) -
 *   historically_disenfranchised_groups: target (organized/identity_locked) -
 *   contemporary_policy_majorities: diffuse target (moderate/constrained) -
 *   living_constitutionalists: excluded voice (organized/mobile) -
 *   constitutional_historians: analytical observer with a secondary
 *   beneficiary position (moderate/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.55).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading: Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "legal/constitutional/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb').
narrative_ontology:cs_kernel_codification('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', fixed_text).
narrative_ontology:cs_authority_grounding('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', lineage).
narrative_ontology:cs_interpretation_layer_present('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb').
narrative_ontology:cs_reading_relation('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', foundational, ratification_fixes_constitutional_meaning).
narrative_ontology:cs_axiom_status(ratification_fixes_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', ratification_fixes_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', foundational, framers_intent_binding_interpretive_standard).
narrative_ontology:cs_axiom_status(framers_intent_binding_interpretive_standard, holdable).
narrative_ontology:cs_axiom_grounding('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', framers_intent_binding_interpretive_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', ratification_fixed_public_meaning).
narrative_ontology:cs_drift_state('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', contemporary_consolidated_originalist_bench, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9fca3cf6-89c0-45ba-88f7-52ffd5fe73bb', '2026-08-04T16:20:00Z').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_legal_establishment).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, states_rights_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, property_rights_coalitions).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_social_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, historically_disenfranchised_groups).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, contemporary_policy_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, constitutional_historians).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, ratification_authority_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, popular_sovereignty_via_article_v).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Supreme Court justices, federal judges, law professors, and allied advocacy organizations that administer the fixed-meaning method: they decide which historical sources count, train the next cohort through law schools and clerkships, staff the bench through aligned appointment pipelines, and produce the scholarship that supplies usable founding-era evidence. Their professional standing, appointment prospects, and institutional networks are constituted by the methodology itself; leaving it would mean forfeiting the authority they have spent careers accumulating inside it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_legal_establishment, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, originalist_legal_establishment, beneficiary).

% State governments, state attorneys general, and decentralist political movements that rely on the narrow scope of federal power the fixed reading protects to preserve spheres of state autonomy. Reserved-powers doctrines and pre-1789 practices carry weight under this reading that they would lose under an evolved one. Their fallback — pursuing the same aims through Congress or constitutional amendment — runs through the very supermajority channels the fixed reading makes the only legitimate route.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, states_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Landowners, investors, and business associations whose preferred limitations on regulation are enumerated in eighteenth-century terms — takings, contracts, commerce limits. The fixed reading insulates those provisions from reinterpretation toward modern regulatory purposes. They fund the interpretive infrastructure and litigate through it, but their wealth gives them parallel channels — state legislatures, international arbitration, private governance — if the constitutional channel narrows.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, property_rights_coalitions, beneficiary,
    powerful, biographical, arbitrage, national).

% Movements asserting rights to healthcare, housing, income security, digital privacy, and environmental protection. The fixed reading's boundary places these claims outside what the ratified text and founding intent can yield, so no court recognition is available; their legislative routes confront the same fixed structure, and Article V amendment is the only door, gated at supermajority thresholds they cannot reach. Exit would mean abandoning the claim to constitutional standing altogether.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_social_rights_claimants, payer,
    organized, biographical, trapped, national).

% Descendant communities of those excluded at the founding — enslaved people, women, Indigenous nations — whose claims to full equal citizenship run through reading the Constitution against its founding-era understandings. Their political identity is fused with the constitutional promise itself; interpreting past the ratifiers' assumptions is not one option among many but the substance of their claim. Waiting on Article V has been the historical experience, and the fixed reading extends the wait indefinitely wherever no amendment passes.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, historically_disenfranchised_groups, payer,
    organized, generational, identity_locked, national).

% Electoral majorities that win ordinary politics but find structural adaptations — new institutional designs, expanded social provision, updated executive-legislative balances — blocked by a structure amendable only at supermajority thresholds. They bear the gap between what they can elect and what the fixed structure permits. Their exit is ordinary politics itself: they keep winning elections inside a structure they cannot reshape.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, contemporary_policy_majorities, payer,
    moderate, biographical, constrained, national).

% Judges, scholars, and advocates who read the text as an aspirational framework whose application evolves with society. When originalist appointees hold the bench, their readings are foreclosed in adjudication regardless of merit; they continue publishing, dissenting, and building alternative pipelines, waiting for appointment cycles to reopen the door. Their objection is registered in dissents and law reviews rather than in holdings.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalists, excluded,
    organized, generational, mobile, national).

% Academic historians whose archival and semantic expertise becomes the load-bearing evidence base once founding-era meaning binds. The arrangement generates demand for their labor and consultancies, and also exposes them to pressure: findings that cut against desired conclusions are attacked as activist, and the volume of demanded certainty exceeds what the archive supports. They watch the structure from the closest vantage point and are among the first to see where evidence ends and selection begins.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_historians, observer,
    moderate, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, constitutional_historians, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_legal_establishment).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one authoritative meaning for the fundamental law across generations and jurisdictions: every interpreter works from the same ratified reference point, lower-court judges and litigants can predict how disputes resolve, and constitutional change travels only through the explicit Article V channel instead of accumulating silently through judicial reinterpretation.
% TRANSFER_FUNCTION: Moves interpretive authority from present-day majorities, litigants, and claimants to the ratifying generation's recorded decisions and to the credentialed interpreters able to reconstruct them; moves constitutional-adaptation capacity from ordinary politics to supermajority amendment; moves legitimacy from evolving consensus to the founding act.
% ABSENT_VOICES: Modern social-rights claimants are defined out of the boundary before anyone hears them — their claims fail at the threshold rather than losing on the merits. Living-constitutionalist judges and scholars are excluded from adjudication whenever originalist appointees hold the bench. Most fundamentally, the governed themselves are absent: no living citizen consented to the fixed meaning; the consenting parties died two centuries ago, and the arrangement binds people who were never asked.
% DISAPPEARANCE_RATIONALE: If the fixed-meaning arrangement vanished overnight, constitutional law would reorganize around evolving interpretive standards within a decade: courts would weigh modern purposes alongside founding text, movements would route claims back toward judicial recognition, the appointment wars that select for methodology would lose their object, and the scholarly infrastructure supplying founding-era evidence would shrink toward ordinary intellectual history. The political coalitions built on the narrow boundary would lose their principal instrument.
% FOUNDING_PROBLEM: Two-layered genealogy. At the founding layer: how to entrench a constitutional settlement against factional capture of the judiciary — fix the reference point so no temporary judicial majority can rewrite the fundamental law. At the modern-revival layer (1970s-80s): how to arrest and reverse the Warren Court's practice of deciding contested policy questions through evolving constitutional readings; originalism was reconstructed as a counter-doctrine to judicial liberalism.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the originalist coalition corroborate the reactive genealogy — the movement's mid-century revival is documented as an organized response to Warren Court jurisprudence (memoirs, funding records, conference proceedings of the 1970s-80s). On whether the founding problem remains live, attestation splits by seat: originalist jurists attest liveness by pointing to continued interpretive disagreement; political scientists measuring constitutional rigidity and amendment rates attest that the settlement-holding function operates as designed. No source outside the benefiting parties attests that binding by long-dead ratifiers remains necessary rather than merely useful to the coalition — that corroboration does not exist, and its absence is itself signal.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.58 — above rope territory, below snare territory — because the arrangement pairs a genuine, widely valued coordination function (a single stable reference point for the fundamental law, adjudicable without recourse to each judge's moral philosophy) with real asymmetric costs: the boundary excludes whole classes of modern claims before hearing them, legitimizes pre-1789 practices an evolved reading would retire, and concentrates interpretive authority in a credentialed cadre. Suppression is 0.55 and structural rather than internalized: alternatives are not eliminated from the culture (rival scholarship thrives) but are foreclosed in the operative venue — adjudication — whenever appointment pipelines deliver an aligned bench; the mechanism is institutional capture of the decision point, not persuasion. Theater_ratio 0.40 reflects the documented law-office-history phenomenon: a substantial share of founding-era argumentation is assembled after the desired conclusion and functions as justification rather than investigation, while a core of serious historical-semantics work remains genuinely load-bearing. Accessibility_collapse is low (0.35): unlike a natural law, understanding this arrangement does not close alternatives — rival readings remain fully available, which is precisely why enforcement machinery (appointments, curricula, confirmation politics) is required. Resistance is high (0.62): sustained academic contestation, oppositional appointment politics, and periodic mass mobilization against originalist-held benches. The measurement series share one grid (points 0-50): base_extractiveness climbs 0.36 to 0.58 as the movement converts intellectual position into adjudicative power and layers policy wins onto the coordination function; theater_ratio climbs 0.22 to 0.40 as the stakes of historical argument rise faster than the archive can bear; suppression_requirement climbs 0.28 to 0.55 as enforcement infrastructure (movement pipeline, aligned nominations, amicus production) matures and hardens. Monotonic extractiveness growth on a stable coordination base is the classic rent-accumulation signature.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat computes a different arrangement than the payer seats do. From inside the establishment, the structure is fidelity: judges are restrained, democracy is protected from unelected life-tenured philosophers, and the historical discipline is honored. From the trapped payer seats, the same structure is closure: claims die at a threshold no argument can cross, and the credential that decides is command of an archive curated by their opponents. Same-level lateral divergence matters too: originalist and living-constitutionalist scholars hold nominally identical academic standing, but the arrangement distributes adjudicative access by methodology, so equal global power yields opposite effective positions. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the establishment (agenda-setter and collector of interpretive authority) sits nearest the beneficiary end; property-rights coalitions sit low-d with arbitrage-grade exits damping even their modest exposure; states-rights advocates are low-d but constrained — their benefit has no substitute channel. Victim declarations drive high directionality: social-rights claimants are trapped (no alternative forum recognizes their claims), placing them near the full-target end; descendant communities are identity-locked to the constitutional promise itself, pinning their exposure high; contemporary policy majorities are moderately exposed — they keep winning ordinary elections inside a structure they cannot reshape. Constitutional historians derive near-symmetric: the arrangement creates demand for their labor (a benefit) while exposing them to politicized attack and certainty-demands the archive cannot meet (a cost). Scope is national: verifying founding-era meaning is hard enough at continental scale that the engine's scope amplification applies modestly to the payer side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested, not dead: the anti-judicial-supremacy concern the modern reading was rebuilt to answer remains arguable, so the mandate has not simply outlived its function. But it has partially mutated — a doctrine built to restrain judges now requires capturing the appointment process to survive, and the movement's maintenance activities (conferences, pipelines, amicus production) increasingly defend the coalition rather than the restraint principle; the theater_ratio series tracks exactly this migration. Classification discipline keeps both halves visible: calling the arrangement pure coordination would erase the boundary-exclusion costs the payer seats demonstrably bear; calling it pure extraction would erase the genuine stabilization function even opponents concede. The tangled_rope claim holds both, and the R5 mismatch consumer finds no dead-problem-plus-world-rearranges flag because the founding problem is contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of kernel us_constitution_1787 — what structurally changes if a sibling reading (living_reading or positivist_reading) is adopted instead?',
    'Comparative classification of the sibling stories: living_reading widens the boundary to admit modern social-rights claims and dissolves pre-1789-practice legitimacy; positivist_reading removes the intent-evidence epistemic demands and recenters binding on text-plus-amendment. The disagreement is located in the temporal index of constitutional meaning.',
    'Adopting a sibling changes the victim set (social-rights claimants exit it under living_reading), the epistemic burden (historical-evidence demands vanish under positivist_reading), and epsilon itself — the three readings are different constraints, not one constraint viewed from angles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    founding_intent_recoverability,
    'Is founding-era intent and public meaning determinate and recoverable rigorously enough to serve as a binding interpretive standard?',
    'Systematic historical-semantics work (corpus linguistics of founding-era texts), blind replication of landmark originalist analyses, and error-rate audits against professional historiography.',
    'If recovery is unreliable, enforcement reduces to credential gatekeeping over who may assert intent; theater_ratio rises further and the arrangement drifts toward extraction sustained by epistemic monopoly rather than by the founding record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_intent_recoverability, empirical, 'Epistemic viability of the binding standard.').

omega_variable(
    dead_hand_legitimacy,
    'Is binding the living by the ratifying generation''s choices a legitimate act of democratic self-binding across generations, or intergenerational domination?',
    'Preference- and theory-dependent: comparative constitutional-rigidity and amendment-rate studies, and whether contemporaries endorse the arrangement as their own continuity or experience it as externally imposed.',
    'Resolved as self-binding, the arrangement reads closer to pure coordination (democratic precommitment); resolved as domination, the extraction component intensifies and the affected set expands to constitutional subjects generally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_legitimacy, conceptual, 'Legitimacy of intergenerational binding.').

omega_variable(
    selective_history_theater_share,
    'Is the measured theatrical share of originalist activity motivated reasoning inside a good-faith method, or strategic deployment of historical ambiguity as cover for predetermined outcomes?',
    'Audit the alignment between analysts'' pre-methodological commitments and their historical conclusions against base rates; track citation patterns for cherry-picking under adversarial review.',
    'A large strategic share raises effective suppression above the structural measure (the gatekeeping is the point; the history is the costume) and pushes the classification toward the snare side; a small share secures the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_history_theater_share, empirical, 'Composition of the theater_ratio.').

omega_variable(
    precedent_second_kernel_drift,
    'Does accumulated judicial precedent now function as a second, unofficial kernel that quietly displaces ratification-fixation while the fixation language is maintained?',
    'Track the share of decided constitutional questions whose outcomes are controlled by precedent rather than by founding-era evidence; measure how often originalist analyses change outcomes versus decorate them.',
    'If precedent governs most outcomes, fixation is maintained theatrically atop a precedent-governed practice — the arrangement is piton-shaped beneath its tangled_rope surface, and the enforcement machinery defends the label rather than the method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_second_kernel_drift, empirical, 'Whether fixation or precedent actually governs outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__originalist_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__originalist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__originalist_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__originalist_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__originalist_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__originalist_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__originalist_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__originalist_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__originalist_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__originalist_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(us_c_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__originalist_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__originalist_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__originalist_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__originalist_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__originalist_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(us_c_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family per the epsilon-invariance principle: the colloquial label 'the Constitution constrains' covers three structurally distinct claims — meaning fixed at ratification with framers' intent binding (this story), meaning evolving with society (living_reading), and meaning identical to enacted text plus formal amendments (positivist_reading). Each has its own epsilon, beneficiary/victim structure, and enforcement profile. This reading currently holds the most adjudicative power, so it exerts structural influence on the positivist sibling's evidentiary environment and forecloses the living sibling within any single adjudicative framework. Linkage here is family bookkeeping, not a claim that the three are one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

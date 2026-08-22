% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Inherited Due-Process Restraint — Living Constitutionalism Reading
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   magna_carta_constraint_authority: the living-constitutionalism reading,
 *   on which Magna Carta establishes an inherited due-process restraint that
 *   binds all subsequent rulers through juridical precedent and evolutionary
 *   interpretation. Per the epsilon-referent rule, extractiveness here
 *   assesses the standing arrangement under contest — the inherited restraint
 *   as this reading sees it, with its actual operation (judicial custody of
 *   meaning, professional practice inside the shield, uneven reach of process
 *   rights) — never some idealized charter the reading would prefer. The two
 *   sibling readings (feudal obsolescence; parliamentary sovereignty) are
 *   separate constraints with their own epsilon values, beneficiaries, and
 *   classifications, authored as their own files; nothing about them is
 *   averaged into this one. KEY AGENTS (by structural relationship):
 *   judiciary (agenda-setter, institutional/identity-locked) — administers
 *   the precedent chain and collects interpretive authority;
 *   crown_and_executive (primary payer, institutional/constrained) — bears
 *   the surrendered discretion; crown_subjects_and_citizens (primary
 *   beneficiary, moderate/constrained) — holds the inherited shield;
 *   parliament (dual payer/beneficiary, institutional/constrained) — drew
 *   power from the original settlement, now bound by its judicial custody;
 *   legal_profession (secondary beneficiary, organized/constrained) —
 *   practices inside the shield; unrepresented_litigants (excluded seat,
 *   powerless/trapped) — nominal holders who cannot reach the shield;
 *   constitutional_historians (analytical observer) — attests the genealogy
 *   from outside the arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.3).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Inherited Due-Process Restraint — Living Constitutionalism Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '8635e782-acb3-4f0d-a7bd-ecc5101a8ce9').
narrative_ontology:cs_kernel_codification('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', fixed_text).
narrative_ontology:cs_authority_grounding('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', lineage).
narrative_ontology:cs_interpretation_layer_present('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9').
narrative_ontology:cs_reading_relation('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', foundational, charter_binds_all_subsequent_rulers).
narrative_ontology:cs_axiom_status(charter_binds_all_subsequent_rulers, holdable).
narrative_ontology:cs_axiom_grounding('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', charter_binds_all_subsequent_rulers, deontological).
narrative_ontology:cs_axiom('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', foundational, charter_meaning_evolves_through_juridical_interpretation).
narrative_ontology:cs_axiom_status(charter_meaning_evolves_through_juridical_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', charter_meaning_evolves_through_juridical_interpretation, conventional).
narrative_ontology:cs_reference_frame('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', charter_as_inherited_binding_law).
narrative_ontology:cs_drift_state('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', contemporary_common_law_world, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8635e782-acb3-4f0d-a7bd-ecc5101a8ce9', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, crown_subjects_and_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, legal_profession).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, crown_and_executive).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, parliament).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, parliament).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, juridical_precedent_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, evolutionary_interpretation_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the inherited restraint: each generation of judges decides what lawful process requires against the facts of new cases, extending or narrowing the shield through published precedent. The bench's authority rests on being custodian of that evolving meaning; judges are trained into the precedent chain, promoted through it, and their professional standing is constituted by fidelity to it. There is no seat outside the chain from which a judge could keep the office while declining the inheritance — exit would mean repudiating the interpretive office itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, beneficiary).

% Holds the discretionary power the inherited restraint bounds: detention, exaction, prosecution, and administrative action must run through lawful process that the office did not author and cannot unilaterally revise. Each officeholder inherits boundaries set by predecessors' defeats in court, within a short political horizon that rewards testing them; testing means litigation the office usually loses and always publicizes. The office does receive offsetting gains — the settlement stabilized the monarchy and lends its actions legitimacy — but the discretion it surrenders is the arrangement's operative cost, and leaving the juridical order entirely would require suspending the courts, a step no holder has taken without regime-level crisis.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, crown_and_executive, payer,
    institutional, biographical, constrained, national).

% Holds the shield: security of person and property against arbitrary state action, inherited across generations without needing to be renegotiated at each change of ruler. The protection is exercised mostly passively — it shapes what officials dare do — and is invoked actively mainly through counsel, which costs money. Ordinary holders can relocate between jurisdictions but carry no guarantee the destination's lineage of the shield is as strong.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, crown_subjects_and_citizens, beneficiary,
    moderate, generational, constrained, national).

% Practices inside the restraint: judicial review, habeas, and administrative challenge are the profession's core work, and the evolving meaning of due process generates the doctrinal questions the bar is paid to argue. Professional status, income, and the bar's self-government all sit inside the juridical order the restraint maintains; exit would mean abandoning the field.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_profession, beneficiary,
    organized, biographical, constrained, continental).

% Legislates in the shadow of the inherited restraint. Historically it was the coalition that first forced written terms on the crown, and its power grew inside that settlement; today it finds statutes read down, struck, or reinterpreted by courts applying an evolved due-process core that Parliament did not enact and can disapply only at constitutional-crisis cost. It both draws authority from the settlement's lineage and chafes under its current judicial custody.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, parliament, beneficiary).

% Would have standing to invoke the shield but lack the resources to reach it: process rights that require counsel, fees, and years of litigation are nominal protections for those without them. They are not seated in the interpretive conversation — no one administering the inheritance consults them on what the shield should reach — and their situation surfaces only when a case happens to be brought on their behalf.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, unrepresented_litigants, excluded,
    powerless, biographical, trapped, national).

% Study the charter's transmission: the 1215 terms, the reissues, the rhetorical afterlives, and the gap between what the text said and what each era made of it. They attest to the genealogy from outside the arrangement — collecting no share of its operation and bearing none of its burdens — and their findings feed both the reading defended here and its rivals.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standing, predictable boundary on sovereign action that rulers and ruled can both plan around: each side knows in advance what process the other is owed, so disputes are adjudicated under shared inherited terms instead of being renegotiated — or fought — at each change of reign.
% TRANSFER_FUNCTION: Moves discretion from rulers to subjects as enforceable process rights, and moves the authority to say what those rights mean, case by case, to the judiciary and the legal profession; across generations it moves the settlement itself, unrenegotiated, from each cohort to the next.
% ABSENT_VOICES: The unfree majority excluded from the 1215 terms — villeins and the landless, for whom the charter's 'free men' did not speak — and their modern analogues: people subject to executive discretion who cannot afford the counsel the shield requires in practice. Both would object that the protection's reach tracks wealth and standing; neither is seated in the interpretive conversation that decides what it covers. The unrepresented_litigants stakeholder carries this seat.
% DISAPPEARANCE_RATIONALE: If the inherited restraint vanished overnight, every common-law polity would reorganize: executive detention, exaction, and prosecution would lose their judicial boundary and be limited only by each holder's prudence; the precedent chain — the operating memory of eight centuries of boundary-setting — would lose its object; parliaments and courts would have to re-found legitimacy from first principles. The shield is not decoration on top of these orders; it is part of their load-bearing structure.
% FOUNDING_PROBLEM: In 1215: a king whose will was the working definition of law — arbitrary exaction, detention without judgment, and courts that answered to the crown alone. The arrangement was built to bind the ruler to lawful process and to make that binding stick beyond the reign that conceded it.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians (the observer seat) attest the founding problem from the documentary record, and contemporary executive-power litigation — brought by civil-liberties organizations and public defenders rather than by any beneficiary of the arrangement — attests that arbitrary sovereign discretion remains a live problem. The executive itself does not attest it; it litigates the other side.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.25): the arrangement's costs fall mainly on the executive — the seat the arrangement was built to bind, which is its designed target, not a captured party — with a mild secondary vector: evolutionary interpretation concentrates the authority to say what the shield means in the bench, and the profession collects work from the shield's invocation. Suppression (0.30, raw and unscaled by power or scope) is structural: the arrangement forecloses discretionary state action through binding precedent. The suppression_requirement series models something distinct — the active enforcement machinery historically required to hold the foreclosure — decaying from baronial arms (0.62 at 1215) through the Stuart-era re-externalization (0.55 at 1600, when the settlement had to be re-won by force) to professional self-sustainment (0.15 at 2026). Enforcement decay here is coordination success, not neglect: the foreclosure persists at 0.30 while the machinery needed to impose it shrank, because the restraint became internalized in precedent, professional socialization, and constitutional convention. Theater (0.27) is low but rising: the due-process core does real work while ceremonial invocation (anniversaries, rhetorical citation, heritage framing) accretes faster than operational use grows. Accessibility collapse (0.50) is rope-typical: alternatives exist — parliamentary revision, constitutional amendment, interpretive retrenchment — but each carries rupture-level cost. Resistance (0.35) reflects eight centuries of episodic boundary-testing by rulers rather than present mass resistance. The 1600 upticks in both extractiveness-adjacent series are historical contingency (a dynasty contesting the settlement), not a recurring cycle. All three series share one time grid (seven points, 1215-2026) so no metric is ever sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   Three institutional seats share one power atom and compute different arrangements. From the bench, the settlement is the interpretive office itself: judicial identity is constituted by custody of the evolving meaning, so the bench experiences the arrangement as the precondition of its own existence — institutional identity-lock, not personal; if the precedent chain broke, the office would not lose a tool, it would lose itself, and the classification would change only if that frame broke (the bench would become an ordinary adjudicator of enacted rules, closer to the parliamentary sibling's world). From the executive, the same settlement is a boundary it inherited, did not author, and can contest only by litigating and usually losing publicly. From parliament, it is genuinely dual: the legislature drew its historical power from the original imposed settlement yet now finds its statutes read through an evolved core it did not enact and can disapply only at constitutional-crisis cost. Subjects experience the shield passively as security of person and property; unrepresented litigants experience the same words nominally — the protection exists on paper and is unreachable without counsel, fees, and years.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries: crown_subjects_and_citizens (hold the shield across generations; d near the beneficiary end), legal_profession (collects practice, status, and income inside the shield; low d), and judiciary (administers the arrangement and mildly collects interpretive authority; low d despite holding the agenda-setter role — the declarations should keep derivation from mistaking administration for target position, which is why the judiciary is declared in both the beneficiary set and the agenda-setter seat). Declared victims: crown_and_executive (bears the surrendered discretion; institutionally powerful but exit-constrained — leaving the juridical order means suspending the courts — so its d sits near the full-target end: the arrangement extracts precisely from the seat least able to arbitrage away; its situation text records the offsetting legitimacy and stability gains so the extraction is not overstated as pure loss), and parliament (bears the restraint on legislation while drawing historical benefit from the settlement — a genuinely mixed position the dual role encodes, sitting mid-scale). Spatial scopes are continental for the interpretive seats (the precedent lineage runs across the common-law world) and national for the executive, citizen, and excluded seats (each experiences the arrangement inside one polity), which feeds the engine's scope modifier. No directionality overrides are used: the beneficiary/victim declarations plus exit options carry the derivation, and the one genuinely mixed seat (parliament) is handled by its dual role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary sovereign discretion — is live, so this is not a mandate outliving its function and no mandatrophy resolution is declared; the R5 mismatch consumer should find status=live paired with verdict=world_rearranges, the non-zombie cell. The rope claim does protective work in both directions: it prevents the arrangement from being misread as pure extraction from rulers (the crown's surrendered discretion is the coordination product the arrangement exists to produce, not a captured rent — and the crown's offsetting legitimacy gains are recorded), while the named mild receipt — interpretive authority accruing to the bench — keeps it from being misread as costless coordination. The series to watch for a future mandatrophy transition is theater_ratio: if the due-process core ever stopped doing work while ceremonial invocation kept growing, the same structure would recompute as theatrical maintenance of a dead settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel magna_carta_constraint_authority — the living_constitutionalism_reading. What would each sibling reading change structurally if instantiated instead?',
    'Authoring the sibling stories: the feudal_obsolescence_reading removes the victim and beneficiary sets entirely (nothing binds, no shield, no parties to name beyond historians); the parliamentary_sovereignty_reading relocates the agenda-setter from the bench to Parliament and makes the restraint''s persistence depend on parliamentary acquiescence and revisable statute rather than precedent.',
    'Classification is reading-indexed: under the feudal sibling the arrangement computes near zero on every metric (a dead compact has no extraction because it has no operation); under the parliamentary sibling the judiciary loses agenda-setter status and the executive''s exit options loosen (statutory revision becomes a live alternative). Cross-reading comparison is valid only at the kernel level, never at the metric level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading this file instantiates, and what each sibling would structurally change.').

omega_variable(
    binding_source_location,
    'Where does the arrangement''s authority flow from — judicial precedent evolving the inherited text (this reading), parliamentary statute absorbing it (the parliamentary sibling), or nowhere (the feudal sibling)?',
    'Not resolvable by evidence alone; it is the kernel contest itself. Partial resolution by institutional observation: which seat''s acts actually change the arrangement''s operation across common-law jurisdictions — courts evolving due-process doctrine, or parliaments enacting and repealing its statutory carriers.',
    'If authority flows through statute, this file''s agenda-setter and victim structure are misassigned and the arrangement should be re-authored under the parliamentary sibling''s constraint_id; if nowhere, the arrangement is ceremonial and theater_ratio is the true headline metric. The parliamentary sibling is authored coexists_with rather than forecloses precisely because a layered hybrid (precedent-evolved core plus revisable statutory carriers) is a coherent single framework some parties actually hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_source_location, conceptual, 'The located disagreement among the three sibling readings: the source and supremacy of the restraint''s authority.').

omega_variable(
    self_sealing_interpretive_stability,
    'The drift_state verdict (stable) is partly self-sealing: a reading whose mechanism is evolutionary interpretation can re-describe any departure from the 1215 text as legitimate interpretation, so its reference frame can never register practice_drift by construction.',
    'An external standard for interpretation versus repudiation — e.g., whether the evolved doctrine still performs the founding function of bounding sovereign discretion, and whether any seat outside the bench (parliament, historians, litigants) can register a change that the bench refuses to count as change.',
    'If the frame is self-sealing, the stable verdict is unfalsifiable from inside this file and the drift data should be read against the siblings'' files, where the same eight-century history computes as codification_collapse (feudal sibling''s frame) or absorption-into-statute (parliamentary sibling''s frame). The stable verdict here is the reading''s honest self-report, not an independent measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_sealing_interpretive_stability, conceptual, 'Whether this reading''s stability verdict is a measurement or a tautology of its own mechanism.').

omega_variable(
    judicial_interpretive_rent_magnitude,
    'Does evolutionary interpretation concentrate extractive interpretive authority in the bench beyond what coordination around an inherited restraint requires?',
    'Comparative institutional analysis across common-law jurisdictions with different interpretive regimes (strong-form judicial review versus parliamentary-supremacy variants): if restraint operation and subject security hold where interpretive custody is more diffuse, the concentrated custody is rent rather than function.',
    'If the rent is material, the arrangement shades from pure coordination toward hybrid — coordinated subjects, a paying executive, and a collecting bench — and effective extraction for the executive seat rises accordingly; if immaterial, the 0.25 extractiveness is mostly coordination cost and the rope claim stands clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_interpretive_rent_magnitude, empirical, 'Size of the bench''s interpretive-authority receipt relative to the coordination need it serves.').

omega_variable(
    shield_reach_vs_litigation_access,
    'Is the shield''s universalization real in operation, or does its reach track access to counsel — making the unrepresented a structurally unprotected class that the reading''s universal language conceals?',
    'Outcome data on process-rights enforcement by litigant resource level; natural experiments from legal-aid and public-defender funding expansions — if enforcement rates converge when access barriers fall, the shield''s reach was access-limited rather than doctrine-limited.',
    'If reach tracks wealth, effective extraction is higher than the base metric suggests for powerless seats, the beneficiary declaration for crown_subjects_and_citizens is only partially earned, and the excluded seat (unrepresented_litigants) migrates toward the victim set — moving the arrangement toward hybrid territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shield_reach_vs_litigation_access, empirical, 'Whether the inherited due-process shield operates universally or is gated by litigation access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_lc_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.08).
narrative_ontology:measurement_basis(magna_lc_tr_t1215, observed).
narrative_ontology:measurement(magna_lc_tr_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement_basis(magna_lc_tr_t1400, observed).
narrative_ontology:measurement(magna_lc_tr_t1600, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1600, 0.18).
narrative_ontology:measurement_basis(magna_lc_tr_t1600, observed).
narrative_ontology:measurement(magna_lc_tr_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1689, 0.15).
narrative_ontology:measurement_basis(magna_lc_tr_t1689, observed).
narrative_ontology:measurement(magna_lc_tr_t1800, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1800, 0.16).
narrative_ontology:measurement_basis(magna_lc_tr_t1800, observed).
narrative_ontology:measurement(magna_lc_tr_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement_basis(magna_lc_tr_t1900, observed).
narrative_ontology:measurement(magna_lc_tr_t2026, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2026, 0.27).
narrative_ontology:measurement_basis(magna_lc_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(magna_lc_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.38).
narrative_ontology:measurement_basis(magna_lc_be_t1215, observed).
narrative_ontology:measurement(magna_lc_be_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1400, 0.33).
narrative_ontology:measurement_basis(magna_lc_be_t1400, observed).
narrative_ontology:measurement(magna_lc_be_t1600, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1600, 0.3).
narrative_ontology:measurement_basis(magna_lc_be_t1600, observed).
narrative_ontology:measurement(magna_lc_be_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1689, 0.26).
narrative_ontology:measurement_basis(magna_lc_be_t1689, observed).
narrative_ontology:measurement(magna_lc_be_t1800, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1800, 0.22).
narrative_ontology:measurement_basis(magna_lc_be_t1800, observed).
narrative_ontology:measurement(magna_lc_be_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1900, 0.21).
narrative_ontology:measurement_basis(magna_lc_be_t1900, observed).
narrative_ontology:measurement(magna_lc_be_t2026, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2026, 0.25).
narrative_ontology:measurement_basis(magna_lc_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(magna_lc_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.62).
narrative_ontology:measurement_basis(magna_lc_su_t1215, observed).
narrative_ontology:measurement(magna_lc_su_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1400, 0.5).
narrative_ontology:measurement_basis(magna_lc_su_t1400, observed).
narrative_ontology:measurement(magna_lc_su_t1600, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement_basis(magna_lc_su_t1600, observed).
narrative_ontology:measurement(magna_lc_su_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1689, 0.35).
narrative_ontology:measurement_basis(magna_lc_su_t1689, observed).
narrative_ontology:measurement(magna_lc_su_t1800, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement_basis(magna_lc_su_t1800, observed).
narrative_ontology:measurement(magna_lc_su_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement_basis(magna_lc_su_t1900, observed).
narrative_ontology:measurement(magna_lc_su_t2026, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2026, 0.15).
narrative_ontology:measurement_basis(magna_lc_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Magna Carta's constraint authority' decomposes into three structurally distinct claims (kernel readings), per the epsilon-invariance principle. This file is the living-constitutionalism member: binding authority flows through juridical precedent and evolutionary interpretation, the executive sits in the victim set, and subjects hold the shield. The feudal-obsolescence member authors the same history as a dead baronial compact (no bindingness, near-zero metrics, no operative parties). The parliamentary-sovereignty member authors bindingness as absorbed into revisable statute, with Parliament as agenda-setter. The members are linked because each is cited as evidence against the others; this member is upstream (highest doctrinal entrenchment in due-process jurisprudence) and structurally influences the downstream two by fixing what any rival account must explain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

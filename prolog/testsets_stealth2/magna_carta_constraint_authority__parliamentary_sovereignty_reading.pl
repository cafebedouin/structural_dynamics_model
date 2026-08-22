% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta's Restraints as Absorbed into Revisable Parliamentary Statute (Parliamentary Sovereignty Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   Under the parliamentary sovereignty reading, Magna Carta's restraints on
 *   sovereign power survive only insofar as Parliament has absorbed them into
 *   statute: the 1297 Confirmatio Cartarum and later enactments carry the
 *   charter's substance, each Parliament may revise or repeal any provision,
 *   and no Parliament binds its successor. The arrangement genuinely
 *   restrains the Crown — prerogative operates only within statutory limits,
 *   enforced by the courts — while concentrating the authority to define the
 *   scope of the inherited liberties in the parliamentary majority. The seats
 *   that hold their protections at the legislature's pleasure — minorities
 *   without durable parliamentary support, the devolved legislatures, and
 *   historically the unrepresented — bear the arrangement's asymmetry. This
 *   story is ONE READING of the kernel magna_carta_constraint_authority; the
 *   sibling readings (living_constitutionalism_reading,
 *   feudal_obsolescence_reading) are separate constraint stories, linked by
 *   network.affects_constraints, with different epsilon values because each
 *   instantiates a different constraint over the same text: the
 *   living-constitutionalism sibling assesses a restraint binding through
 *   precedent independently of Parliament (a different victim structure, with
 *   protections enforceable against majoritarian revision); the
 *   feudal-obsolescence sibling assesses a historical artifact with no
 *   parties. The epsilon authored here (0.52; referent: the standing
 *   arrangement of charter restraints as revisable statute, assessed by this
 *   reading's own lights) is moderate because both the coordination function
 *   and the revisability asymmetry are structurally real.
 *
 * KEY AGENTS:
 *   - westminster_parliament: agenda-setter and principal beneficiary (institutional/arbitrage) — inherits, administers, and may unmake any absorbed provision; collects the guardianship standing
 *   - majoritarian_electorate: beneficiary (powerful/constrained) — the mediated popular will; the one force the arrangement does not subject to revision
 *   - crown_executive_branch: formal target of the restraints (institutional/trapped) — bound by the statutory corpus its own party usually commands through the Commons
 *   - minorities_without_entrenchment: primary extraction-bearing seat (powerless/trapped) — protections held at the legislature's pleasure, no recourse forum
 *   - devolved_legislatures: subordinate legislatures (institutional/trapped, regional scope) — settlements held at Westminster's sufferance
 *   - common_law_judiciary: dual-positioned administrator (institutional/identity_locked) — collects the interpretive function; bears the revisability of its own rulings
 *   - unrepresented_subjects_of_westminster: excluded seat (powerless/trapped) — governed by the inherited authority without a voice in its revision
 *   - constitutional_theorists: analytical observer (analytical/analytical) — holds and contests the rival readings from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.52).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta's Restraints as Absorbed into Revisable Parliamentary Statute (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '7c761681-49b1-4527-8408-af337e8851ff').
narrative_ontology:cs_kernel_codification('7c761681-49b1-4527-8408-af337e8851ff', fixed_text).
narrative_ontology:cs_authority_grounding('7c761681-49b1-4527-8408-af337e8851ff', practice).
narrative_ontology:cs_interpretation_layer_present('7c761681-49b1-4527-8408-af337e8851ff').
narrative_ontology:cs_reading_relation('7c761681-49b1-4527-8408-af337e8851ff', magna_carta_constraint_authority__living_constitutionalism_reading, influences).
narrative_ontology:cs_reading_relation('7c761681-49b1-4527-8408-af337e8851ff', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('7c761681-49b1-4527-8408-af337e8851ff', foundational, no_parliament_binds_its_successor).
narrative_ontology:cs_axiom_status(no_parliament_binds_its_successor, holdable).
narrative_ontology:cs_axiom_grounding('7c761681-49b1-4527-8408-af337e8851ff', no_parliament_binds_its_successor, conventional).
narrative_ontology:cs_axiom('7c761681-49b1-4527-8408-af337e8851ff', foundational, charter_authority_survives_through_statutory_absorption).
narrative_ontology:cs_axiom_status(charter_authority_survives_through_statutory_absorption, holdable).
narrative_ontology:cs_axiom_grounding('7c761681-49b1-4527-8408-af337e8851ff', charter_authority_survives_through_statutory_absorption, conventional).
narrative_ontology:cs_axiom('7c761681-49b1-4527-8408-af337e8851ff', secondary, judicial_invalidation_of_statutes_impermissible).
narrative_ontology:cs_axiom_status(judicial_invalidation_of_statutes_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7c761681-49b1-4527-8408-af337e8851ff', judicial_invalidation_of_statutes_impermissible, conventional).
narrative_ontology:cs_reference_frame('7c761681-49b1-4527-8408-af337e8851ff', charter_as_parliamentary_statute).
narrative_ontology:cs_drift_state('7c761681-49b1-4527-8408-af337e8851ff', contemporary_post_brexit_restoration, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7c761681-49b1-4527-8408-af337e8851ff', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, westminster_parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majoritarian_electorate).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_judiciary).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minorities_without_entrenchment).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, devolved_legislatures).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_judiciary).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, successor_parliament_unbinding_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherits the charter's authority in statutory form: the 1297 Confirmatio Cartarum and later enactments absorbing the charter's substance sit on the statute book. Each Parliament administers the absorbed restraints, interprets them through legislation, and may revise or repeal any provision — no Parliament binds its successor. The arrangement concentrates in this seat both the keeping of the restraints and the power to unmake them, along with the public standing of being guardian of ancient liberties.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, westminster_parliament, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, westminster_parliament, beneficiary).

% Holds effective sovereignty through elections: it installs and removes the Commons, and its will — as mediated through Parliament — is the one force the arrangement does not subject to revision. The restraint on the Crown serves accountable government this seat directs, and it bears little of the arrangement's cost: any protection it enjoys can be withdrawn only by a majority, which it is. Voters cannot opt out of the constitutional structure itself, only change who staffs it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majoritarian_electorate, beneficiary,
    powerful, biographical, constrained, national).

% Bears the absorbed restraints directly: prerogative powers operate only so far as statute leaves them, and the executive cannot act against an Act of Parliament. It has no exit from the legal order that binds it. A recurring structural wrinkle: the executive's party usually commands the Commons, so the body keeping and revising the restraint on the executive is typically one the executive leads.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive_branch, payer,
    institutional, generational, trapped, national).

% Holds whatever protections the statute book currently provides — religious minorities, unpopular dissenters, non-citizens, any group without durable parliamentary support — and holds them at the legislature's pleasure: each protection stands only until a future majority revises or repeals it. Courts must apply even a repealing statute, so there is no forum in which this seat can hold a protection against the majority. Exit means leaving the jurisdiction entirely, at a cost most members of this seat cannot pay.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minorities_without_entrenchment, payer,
    powerless, biographical, trapped, national).

% The Scottish Parliament, Senedd, and Northern Ireland Assembly exercise real legislative power over regional matters, but their existence, powers, and boundaries are held at Westminster's sufferance: Westminster can legislate on devolved matters without consent and can alter or abolish the settlements, and the devolved bodies have no unilateral exit. Their position is a standing, live demonstration of what it is to hold an institutional settlement at another body's pleasure.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, devolved_legislatures, payer,
    institutional, generational, trapped, regional).

% Administers the absorbed restraints in litigation: it applies the statutory corpus, develops its interpretation, and in notable cases has asserted limits on prerogative power. Its authority is delegated and revisable — Parliament can override any interpretation by express statute, and the courts accept they cannot strike down Acts. The profession is constituted through guardianship of the inherited liberties: its self-understanding, its great precedents, and its professional identity are built on the charter tradition it enforces but cannot ultimately secure against the majority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_judiciary, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_judiciary, payer).

% Those governed by the inherited authority without a vote in the revising body. Historically: the American colonists, who invoked the charter against parliamentary taxation and were answered with the Declaratory Act 1766; colonial subjects across the Empire; the propertyless and women before franchise reform. Today: disenfranchised residents and non-citizens who live under the restraints but cannot vote on their revision. This seat has never held a place in the arrangement's conversation; its characteristic exits have been emigration or revolution.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_subjects_of_westminster, excluded,
    powerless, biographical, trapped, global).

% Analyzes the arrangement from outside its operation: traces the absorption of the charter into statute, articulates and contests the doctrine of parliamentary sovereignty, and holds the rival readings of what the charter's survival means. This seat gains nothing from the arrangement and loses nothing to it; its product is the interpretive record the other seats draw on in dispute.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, westminster_parliament).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restraint of executive power through law: the charter's guarantees — lawful judgment, due process, limits on arbitrary seizure and taxation — operate as ordinary statute binding the Crown, administered by the courts and kept current by a legislature with full revisionary authority. The arrangement also coordinates the succession of the restraint itself: each Parliament inherits the restraints in statutory form, so restraint survives every change in who holds sovereignty.
% TRANSFER_FUNCTION: Two transfers run through one structure. Discretion moves from the Crown to law: prerogative yields to statute, enforced by courts. And the authority to define the scope of the inherited liberties moves from courts and rights-holders to the parliamentary majority: every protection exists only so long as the majority declines to revise it, and the majority's own will is the one thing the arrangement does not subject to revision.
% ABSENT_VOICES: Those governed by the inherited authority without a seat in the revising body: historically the American colonists, who invoked the charter against parliamentary taxation and were answered with the Declaratory Act 1766; colonial subjects; the unenfranchised before franchise reform. Today: disenfranchised residents and non-citizens who live under the restraints but cannot vote on their revision. Advocates of entrenchment also hold no institutional seat — the doctrine leaves no forum in which a claim that Parliament cannot revise a provision can even be heard.
% DISAPPEARANCE_RATIONALE: If the absorbed restraints and their revisionary administration vanished overnight, the executive would stand unbound until a successor instrument was enacted; the courts would lose the statutory corpus they apply daily; minorities' protections would lapse entirely, since they hold no source of right outside the statute book; the devolved settlements would float free of their grantor; and the authority to define rights would have to be re-founded from scratch. The constitutional order rearranges around the absence.
% FOUNDING_PROBLEM: The 1215 compact answered baronial grievances against arbitrary royal exaction and imprisonment by placing the King under law. The absorption settlement answered the successor problem: how the restraints survive the transfer of sovereignty from Crown to Parliament — keeping the executive under law after the maker of law changed hands.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the UK Supreme Court's unanimous judgment in Miller (No. 2) (2019) attests that executive attempts to escape restraint remain live — the court asserted the limit while the doctrine gives it no sovereignty to gain, so the attestation costs the court rather than paying it; minority-rights organizations and devolved governments litigate the revisability from seats that bear it; constitutional historians working outside the Westminster tradition trace the restraint problem continuously from Runnymede to the present. The arrangement's own machinery — Parliament and the government of the day — is not the source of the attestation.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.52 at interval end) because the arrangement genuinely restrains the executive through law while placing every protection at the majority's unilateral disposal — the coordination is real and so is the asymmetry. The trajectory is U-shaped: extractiveness peaked in the oligarchic eighteenth century, when the mediating majority was a narrow franchise and the revisability asymmetry was exercised hardest against dissenters and colonists; it fell as franchise reform widened the mediating majority — the historical coalition route out of the payer seat was entry into the electorate (Chartism, suffrage movements, colonial representation), so the arrangement's asymmetry responds to who counts as the majority; it reached its floor under EU primacy (1972-2020), when protections gained a backstop Westminster could not unilaterally revise; and it has risen since restoration as the revisability of minority and devolved protections was re-demonstrated. Suppression (0.58) is a raw structural property, unscaled by power or scope: the doctrine excludes rival constraint authorities — entrenchment, judicial invalidation of statutes, popular ratification — from the legal framework while leaving them visible as political proposals, which is why accessibility_collapse (0.6) sits well below a natural law's near-total collapse: the alternatives are understood and repeatedly proposed, only foreclosed. Theater (0.56) is high and rising because the charter's distinct operative content has been almost wholly absorbed into statute re-enacted on independent grounds; what remains distinctively charter-shaped is increasingly ceremony, anniversary, and rhetorical invocation over a statutory base. Resistance (0.5) is persistent rather than episodic: the American Revolution was the maximal historical rejection of this reading, and the contemporary record runs from academic living-constitutionalism through devolutionary grievance to judicial dicta (Jackson, Thoburn, Miller) testing the doctrine's edges. The claimed type — tangled_rope — is authored from the structure: a genuine coordination function (executive restraint through law, kept current by a competent legislature) AND an extraction asymmetry (revisability borne by seats without durable parliamentary support), held together by active enforcement (courts applying the corpus, parliamentary procedure maintaining it). Metrics are authored independently of the claim; where the engine computes a different type for a seat, that divergence is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   From the Parliament seat the arrangement is the legitimate inheritance and exercise of constraint authority: the restraints are kept because Parliament keeps them, and their revisability is not a defect but the constitution's engine. From the minorities seat the same structure is protection held at the majority's pleasure: the restraint on the executive is real, but it offers this seat nothing it can hold against the majority. From the Crown/executive seat it is binding restraint administered by a body its own party usually commands: restraint and self-restraint blur. From the judiciary seat it is a guardianship without final authority: the profession enforces the inherited liberties while the doctrine denies it the last word. The engine computes these per-seat classifications from the structural data; the divergence between the agenda-setter seat and the payer seats is the story's central perspectival fact, and it is not adjudicated by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to directionality as follows. westminster_parliament sits near the beneficiary end: it collects the constraint authority and the guardianship standing, and it is exempt from the restraints it administers. majoritarian_electorate also sits low: the restraint serves the government it directs, and revisability costs it nothing while it remains the majority. common_law_judiciary sits near symmetric: it collects the interpretive function over the inherited liberties (benefit) while its own rulings are revisable by express statute (cost) — the dual role is genuine. crown_executive_branch sits near the target end: it bears the restraint fully, with no exit from the legal order that binds it. minorities_without_entrenchment and devolved_legislatures sit at or near the full-target end: everything they hold is revisable at the majority's pleasure, with no recourse forum and no exit. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already differentiate every seat, and the available overrides key on power atoms — Parliament, the Crown, and the judiciary all occupy the institutional atom with opposite structural relationships, so an institutional-level override would blur exactly the divergence this story exists to measure. The receipt surface names westminster_parliament: the revisionary authority over the charter's content — the arrangement's extractive gain — demonstrably accrues to the institution that administers and exercises it; the electorate benefits through that seat, but the gains land in the institution. Fixing is prohibitive: the only actor with authority to entrench the restraints is the actor whose entrenchment would have to bind itself, and the doctrine denies the efficacy of self-binding — every fix attempted (written constitutions, entrenched bills of rights, referendum entrenchment) has required refounding the source of authority, not passing an Act.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping the executive under law through a change in the holder of sovereignty — is live, so this is not a zombie arrangement. The mandatrophy risk sits one level down: the charter's distinct function (a compact restraint carrying its own authority) has atrophied into absorption and symbol, while the constraint function it carried (executive restraint through law) remains live under new administration. The classification prevents two mislabelings. Calling the arrangement pure extraction would miss the genuine coordination: the executive is in fact restrained by law, the restraints are kept current by a competent legislature, and every seat including the payers receives the rule-of-law core. Calling it pure coordination would miss the asymmetry: the same structure that restrains the executive places every minority protection at the majority's unilateral disposal, and identifiable seats bear that with no recourse. The tangled_rope claim keeps both halves on the table. The sibling readings bracket it: under the living-constitutionalism sibling the revisability asymmetry — this arrangement's extraction — collapses, because protections become enforceable against majoritarian revision; under the feudal-obsolescence sibling both the coordination and the asymmetry dissolve into ordinary statute and historical memory. This reading is the middle structure, and its moderate extractiveness is the signature of the middle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_of_authority,
    'This constraint is the parliamentary_sovereignty_reading of kernel magna_carta_constraint_authority: the contest is over where the charter''s constraint authority sits — parliamentary enactment (this reading), juridical precedent (living_constitutionalism_reading), or nowhere (feudal_obsolescence_reading). What would adopting a sibling change structurally?',
    'The siblings are separate constraint stories linked by network.affects_constraints; the dispute resolves jurisprudentially or politically — a court asserting authority to disapply an Act on charter grounds (Factortame-style) would shift operative authority toward the living-constitutionalism reading; a repeal of the 1297 Confirmatio without replacement would shift toward feudal obsolescence.',
    'Under living constitutionalism this reading''s revisability asymmetry collapses — protections become enforceable against majoritarian revision and the victim set shrinks; under feudal obsolescence the constraint''s beneficiaries and victims dissolve entirely and the story reduces to a historical artifact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_authority, conceptual, 'Kernel contest: the locus of the charter''s constraint authority across three readings.').

omega_variable(
    revisability_structure_vs_exercise,
    'Is the arrangement''s extractiveness the revisability itself (structural — borne by everyone whose protections are held at sufferance, whether or not revision occurs) or its exercise (contingent — borne only when majorities actually revise against minorities)?',
    'Comparative statutory history: measure whether protections for groups lacking durable parliamentary support are revised or repealed at higher rates than protections for supported groups, controlling for provision age.',
    'If structural, the moderate measured value understates standing exposure and epsilon is constant across political eras; if contingent, epsilon tracks political history and the 1972-2020 dip was real relief rather than suspension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisability_structure_vs_exercise, empirical, 'Whether extraction lies in revisability as such or in its exercise.').

omega_variable(
    executive_cohold_of_revision,
    'Does the executive''s usual command of the House of Commons mean the revisionary authority over the absorbed restraints is effectively co-held by their formal target — is the mediated popular will in practice the government''s will?',
    'Trace revision episodes of executive-restraining and rights-protecting provisions: classify the initiating seat (government bill, backbench bill, popular campaign) and passage dynamics under party discipline.',
    'If co-held, the majoritarian_electorate seat''s beneficiary position is more nominal than real, the coordination function is weaker than it appears, and seat divergence between the electorate and the executive compresses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_cohold_of_revision, empirical, 'Whether the revising majority is genuinely popular or executive-directed.').

omega_variable(
    absorption_independence_counterfactual,
    'How much of the charter''s operative content would exist in statute anyway, re-enacted for reasons independent of the charter lineage — is the surviving charter function doing distinct work, or is it a symbolic layer over content with independent statutory grounding?',
    'Counterfactual statutory genealogy: for each operative provision tracing to charter content, assess whether independent doctrinal or practical grounds would have produced it; provisions with full independent grounding contribute theater, not function.',
    'High independent grounding raises theater_ratio further and moves this reading''s empirical content toward the feudal_obolescence_reading while leaving its formal structure intact — the constraint survives but the charter''s distinct identity does not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_independence_counterfactual, empirical, 'Whether the absorbed content is distinctively charter-derived or independently grounded.').

omega_variable(
    entrenchment_foreclosure_contingency,
    'Is the exclusion of entrenchment alternatives a structural impossibility (the constitution cannot entrench — the orthodox no-parliament-binds-successor doctrine) or a contingent, maintained choice (Parliament could entrench and chooses not to, per the Jackson dicta strand)?',
    'Jurisprudential: a court entertaining the Jackson dicta strand and giving an entrenched provision priority over a later contrary Act would resolve it toward contingency; continued doctrinal orthodoxy maintains the structural reading.',
    'If contingent, the suppression of alternatives is self-imposed and maintained — the arrangement persists by ongoing choice of its beneficiary, strengthening the coordinated-extraction reading; if structural, the exclusion is closer to a fixed feature and the revisability is a genuine constitutional constant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_foreclosure_contingency, conceptual, 'Whether the entrenchment foreclosure is structural or a maintained choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1689, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1689, 0.14).
narrative_ontology:measurement_basis(magn_tr_t1689, observed).
narrative_ontology:measurement(magn_tr_t1766, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1766, 0.22).
narrative_ontology:measurement_basis(magn_tr_t1766, observed).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1832, 0.3).
narrative_ontology:measurement_basis(magn_tr_t1832, observed).
narrative_ontology:measurement(magn_tr_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1911, 0.38).
narrative_ontology:measurement_basis(magn_tr_t1911, observed).
narrative_ontology:measurement(magn_tr_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1972, 0.42).
narrative_ontology:measurement_basis(magn_tr_t1972, observed).
narrative_ontology:measurement(magn_tr_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1998, 0.5).
narrative_ontology:measurement_basis(magn_tr_t1998, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2025, 0.56).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1689, 0.48).
narrative_ontology:measurement_basis(magn_be_t1689, observed).
narrative_ontology:measurement(magn_be_t1766, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1766, 0.56).
narrative_ontology:measurement_basis(magn_be_t1766, observed).
narrative_ontology:measurement(magn_be_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1832, 0.5).
narrative_ontology:measurement_basis(magn_be_t1832, observed).
narrative_ontology:measurement(magn_be_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1911, 0.42).
narrative_ontology:measurement_basis(magn_be_t1911, observed).
narrative_ontology:measurement(magn_be_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1972, 0.38).
narrative_ontology:measurement_basis(magn_be_t1972, observed).
narrative_ontology:measurement(magn_be_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1998, 0.46).
narrative_ontology:measurement_basis(magn_be_t1998, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1689, 0.52).
narrative_ontology:measurement_basis(magn_su_t1689, observed).
narrative_ontology:measurement(magn_su_t1766, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1766, 0.6).
narrative_ontology:measurement_basis(magn_su_t1766, observed).
narrative_ontology:measurement(magn_su_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1832, 0.54).
narrative_ontology:measurement_basis(magn_su_t1832, observed).
narrative_ontology:measurement(magn_su_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1911, 0.46).
narrative_ontology:measurement_basis(magn_su_t1911, observed).
narrative_ontology:measurement(magn_su_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1972, 0.4).
narrative_ontology:measurement_basis(magn_su_t1972, observed).
narrative_ontology:measurement(magn_su_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement_basis(magn_su_t1998, observed).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(magn_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (magna_carta_constraint_authority — the persisting commitment that Magna Carta's restraints bind subsequent sovereign power), three readings, three constraints. This story instantiates the parliamentary_sovereignty_reading: the restraints survive only as absorbed into revisable statute; epsilon is moderate (0.52) because genuine executive restraint and a genuine revisability asymmetry coexist in one structure. The living_constitutionalism_reading sibling instantiates a restraint binding through juridical precedent independently of Parliament — its epsilon is authored over a different arrangement (liberties enforceable against majoritarian revision) with a correspondingly different victim set. The feudal_obsolescence_reading sibling instantiates a historical artifact with no binding force — no parties, no extraction. Upstream/downstream structure: the sovereignty doctrine (this reading) is the official judicial position and shapes the operating environment of both siblings — it is what living constitutionalism must argue against and what feudal obsolescence is partially vindicated by. All three files link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

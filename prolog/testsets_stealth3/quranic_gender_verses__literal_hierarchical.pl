% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses - Literal Hierarchical Reading (Verses 4:11, 2:282, 4:34 as Direct Timeless Legal Ordinance)
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'quranic
 *   gender verses': the literal_hierarchical reading, under which verses 4:11
 *   (inheritance shares), 2:282 (testimony weighting), and 4:34 (male
 *   guardianship and household authority) operate as direct, timeless,
 *   legally binding divine ordinance with no interpretive mediation. Under
 *   this reading the standing arrangement is the classical-fiqh application:
 *   male kin receive doubled estate portions, women's testimony carries a
 *   formal discount in major transactions, and women live under male
 *   guardianship structures. The reading's own lights register the
 *   differentials as divinely apportioned complementarity rather than
 *   injustice, but the measured magnitudes are what epsilon records: the
 *   transfers of property, epistemic standing, and autonomy are large,
 *   unconditional on the receiving side, and enforced. Sibling readings
 *   (contextual_egalitarian, progressive_abrogation) instantiate DIFFERENT
 *   constraints from the same verses and are separate files linked through
 *   the network section; this file contains only the literal reading, with
 *   its own stable epsilon, per the epsilon-invariance principle. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (real coordination function plus asymmetric extraction plus active
 *   enforcement) while the authored metrics describe heavily extractive,
 *   increasingly enforced operation - the engine measures that divergence;
 *   the claim is not tuned to the metrics.
 *
 * KEY AGENTS:
 *   - male_household_heads: Primary beneficiary (organized/arbitrage) - collect doubled shares and guardianship authority without administering anything
 *   - religious_jurists_courts: Agenda-setter (institutional/identity_locked) - interpret, adjudicate, and enforce; their office exists only so long as the verses stay directly operative
 *   - female_heirs: Primary target (powerless/trapped) - bear the estate-division differential with no exit short of family rupture
 *   - wives_under_guardianship: Primary target (powerless/identity_locked) - bear the guardianship hierarchy; faith, family, and subsistence are fused
 *   - women_commercial_participants: Secondary target (moderate/constrained) - bear the testimony discount with partial workarounds
 *   - reformist_scholarship_movement: Contested insider paying enforcement costs (organized/identity_locked) - bears takfir and exclusion for contesting the reading from within
 *   - muslim_communities: Diffuse beneficiary with diffuse costs (organized/constrained) - receive dispute-minimizing rules, distribute costs across half their members
 *   - grassroots_women_conservative_jurisdictions: Excluded voice (powerless/trapped) - live the rules with no channel into the conversation
 *   - comparative_law_academics: Analytical observer (analytical/analytical) - hold the historical record all seats argue over
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.72).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.85).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.72).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses - Literal Hierarchical Reading (Verses 4:11, 2:282, 4:34 as Direct Timeless Legal Ordinance)").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'c39cb563-3c69-4ad9-be68-73fb6e5ac92e').
narrative_ontology:cs_kernel_codification('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', fixed_text).
narrative_ontology:cs_authority_grounding('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', lineage).
narrative_ontology:cs_interpretation_layer_present('c39cb563-3c69-4ad9-be68-73fb6e5ac92e').
narrative_ontology:cs_reading_relation('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', foundational, gender_rulings_bind_timelessly_as_revealed).
narrative_ontology:cs_axiom_status(gender_rulings_bind_timelessly_as_revealed, holdable).
narrative_ontology:cs_axiom_grounding('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', gender_rulings_bind_timelessly_as_revealed, theological).
narrative_ontology:cs_axiom('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', secondary, differentiated_rights_express_divine_wisdom_not_male_interest).
narrative_ontology:cs_axiom_status(differentiated_rights_express_divine_wisdom_not_male_interest, holdable).
narrative_ontology:cs_axiom_grounding('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', differentiated_rights_express_divine_wisdom_not_male_interest, theological).
narrative_ontology:cs_reference_frame('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', timeless_direct_legal_ordinance).
narrative_ontology:cs_drift_state('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', contemporary_reform_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c39cb563-3c69-4ad9-be68-73fb6e5ac92e', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_jurists_courts).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, muslim_communities).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, wives_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_commercial_participants).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, reformist_scholarship_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, muslim_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collect the doubled inheritance portions assigned to male kin, exercise guardianship authority over female relatives' marriage contracting and, in classical application, household discipline, and are exempt from the testimony discount's costs. The arrangement requires no effort from them to maintain; leaving it is always available and costs nothing. They carry maintenance obligations toward female dependents, which the reading counts as the balancing side of the differential.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    organized, generational, arbitrage, global).

% Interpret the verses, issue rulings, staff sharī'a courts and personal-status registries, and train the transmitters who reproduce the reading. Their scholarly office rests on the verses remaining directly legally operative; treating the rulings as historically superseded would dissolve the basis of their authority. They sanction deviation as innovation or disbelief, and their adjudication role gives them standing, income, and gatekeeping power over marriage, divorce, and estate division.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_jurists_courts, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive fixed half-share portions of estates relative to brothers under 4:11 as literally applied. Division is executed by courts or family councils regardless of their preference; refusing the division forfeits family standing, and leaving the community altogether carries apostasy costs and total family rupture. Their receipt of any share at all is, historically, an improvement over the pre-Islamic exclusion the verses displaced.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_heirs, payer,
    powerless, biographical, trapped, global).

% Live under the guardianship provisions of 4:34 as classically applied: male wardship in marriage contracting, hierarchical authority in the household including the nushuz discipline sequence, and dependency on male maintenance that is contingent on continued marital compliance. Their faith identity, family belonging, and economic security are fused; exit means apostasy, destitution, or both.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, wives_under_guardianship, payer,
    powerless, biographical, identity_locked, global).

% In literal-application jurisdictions, major financial transactions per 2:282 require either two female witnesses or one female witness alongside one male. They work around the standing discount through written documentation, agency delegation, or securing a male co-witness, which softens but does not remove the epistemic discount attached to their word in formal settings.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_commercial_participants, payer,
    moderate, biographical, constrained, regional).

% Receive dispute-minimizing estate-division rules, a defined household order, and a visible marker of communal fidelity to revelation. They bear the costs distributed across half their members and the friction between the applied rules and international human-rights frameworks, which lands on the community's institutions and emigrants.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, muslim_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, muslim_communities, payer).

% Scholars and coalitions working inside the tradition (contextualist exegesis, maqasid-based reasoning, Musawah-type organizing) who contest the literal application of these verses. They bear heresy and disbelief accusations, exclusion from official councils and endowed posts, and in some jurisdictions legal jeopardy. Exiting into purely secular critique would surrender the internal audience they exist to address, so their position is locked by their own project.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, reformist_scholarship_movement, payer,
    organized, generational, identity_locked, global).

% Women living the rules' daily operation in jurisdictions where public advocacy is unsafe or unheard. They experience the inheritance divisions, witness requirements, and guardianship hierarchies directly but have no channel into either the juristic councils that fix application or the reformist conversations that contest it.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, grassroots_women_conservative_jurisdictions, excluded,
    powerless, immediate, trapped, local).

% Document the verses' application across jurisdictions and centuries, reconstruct the occasion-of-revelation context, and compare personal-status codes. They neither collect from nor pay into the arrangement; they produce the historical and legal record that every other seat argues over.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, comparative_law_academics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides determinate fractional rules for estate division that preempt succession disputes, a redundancy mechanism for verifying commercial claims in a low-documentation environment, and a defined household authority-and-maintenance structure in a tribal economy with exposed unattached women.
% TRANSFER_FUNCTION: Moves property (the share differential between male and female heirs), legal-epistemic standing (the testimony weighting), and household decision authority from women to male kin; and moves interpretive authority, adjudication standing, and gatekeeping control over marriage, divorce, and estates to the juristic establishment.
% ABSENT_VOICES: Women subject to the rules were absent from the juristic councils where consensus on application crystallized; grassroots women in conservative jurisdictions today lack any safe channel into the debate; internal dissenters who speak face disbelief accusations that function as expulsion from the conversation.
% DISAPPEARANCE_RATIONALE: Personal-status law across dozens of states, sharī'a court procedure, family inheritance practice, and the juristic profession's entire authority structure are organized around these provisions operating as stated. Overnight removal would force wholesale reconstruction of inheritance law, reopen contested successions everywhere, and strip the establishment of its interpretive office.
% FOUNDING_PROBLEM: Seventh-century Arabian practice excluded female heirs from inheritance entirely, commercial disputes lacked reliable verification in a predominantly oral economy, and unattached women in a tribal order had no protected legal position. The verses addressed all three: they granted women shares at all, paired witnesses to catch error, and channeled protection through defined guardianship.
% FOUNDING_PROBLEM_CORROBORATION: The juristic establishment attests the founding problem as live, citing the verses' permanent validity. Corroboration from outside the benefiting parties: historians of late-antique Arabia attest the pre-Islamic exclusion of female heirs that the verses remedied; contemporary corroboration of the superseded-function reading comes from reform-movement research programs (Musawah's knowledge-building), UN CEDAW treaty-body reviews of Muslim-majority states, and academic Islamic-law scholarship (e.g., work on pre-Islamic inheritance practice and on the construction of the juristic tradition) written from outside the establishment.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the differentials are unconditional on the receiving side: the doubled share accrues whether or not the recipient discharges the maintenance obligations offered as compensation, while women's guaranteed maintenance is contingent on continued compliance. Suppression is higher still (0.85) because persistence depends on active enforcement machinery - apostasy statutes in several jurisdictions, takfir directed at reformist scholars, family-totalizing social sanction - not on participant preference. Theater is low-to-moderate (0.30): the rules genuinely execute (estates are actually divided, witness rules actually applied), with a growing performative component in modernity as literal claims are invoked in venues where practice is already mediated or performed for international audiences. The temporal series run on ONE shared nine-point grid (650-2025) so every metric is authored at every examined time point. Extractiveness rises through classical consolidation (fiqh systematization embeds the differentials fully by 1000), stays flat through the post-classical taqlid era, dips under colonial legal pluralism and mid-century personal-status codification in reformist states (1950 trough), then returns toward the classical level as revivalist movements re-tighten application in significant jurisdictions. The suppression series is the enforcement-capacity trajectory the story specifically traces: community-consensus self-enforcement in the formative period (0.45), hardening through classical orthodoxy-formation, a dip when colonial administrations displaced qadi courts, then pronounced ratcheting from 1950 to 2025 as nation-states codified enforcement and revivalist infrastructure (hisbah institutions, apostasy statutes, coordinated takfir networks) matured against modern dissent. Neither series oscillates; the two dips are external-shock effects (colonial codification, state reform), not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structural data. From the jurist seat the arrangement is genuine divine coordination it stewards - the coordination function is real, the extraction is invisible as extraction because the frame names it wisdom. From the female_heir and wives_under_guardianship seats the same verses compute as enforced extraction with suppressed exits and identity-fused lock-in. From the male_household_heads seat the arrangement is a mild, low-salience benefit requiring no defense. The engine computes this per-seat divergence from the declared roles, power atoms, and exit options; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. male_household_heads sit nearest the beneficiary pole (d near 0.05-0.1): full subsidy, arbitrage-grade exit. religious_jurists_courts sit near the beneficiary pole (d near 0.1) with agenda control amplifying their position. muslim_communities sit near symmetric (d near 0.45): cohesion benefits spread across all members while costs concentrate on half. female_heirs (d near 0.9) and wives_under_guardianship (d near 0.95) sit near the full-target pole - trapped and identity_locked exits push them to the extreme. women_commercial_participants (d near 0.75) are targets with partial arbitrage softening the discount. reformist_scholarship_movement (d near 0.85) are targets of the enforcement machinery specifically. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options already produces the correct relationships for every seat, and the guidance reserves overrides for cases the derivation gets wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting unattached women in a tribal economy, dispute-free estate division, error-catching in an oral commercial culture) was real and the verses were a genuine advance on the jahiliyya baseline they displaced - which is exactly why a pure-snare reading fails: the coordination core is not cover, it delivered. But the specific mechanisms are now argued by reformists to be historically superseded while literalists hold them permanently binding, so the founding problem's status is honestly CONTESTED, not dead - and because status is contested rather than dead, the dead-plus-world_rearranges mismatch flag does not fire. The mandatrophy-resolved flag is deliberately NOT set: the mandate has not plainly outlived its function everywhere; in reformed jurisdictions its protective function is carried by replacement institutions, while in literal-application jurisdictions the arrangement persists substantially through enforcement and identity rather than through the original protective necessity. Claiming tangled_rope rather than snare preserves the real coordination achievement; authoring high extraction and rising suppression rather than rope-level values preserves the asymmetric, enforced transfer. Both halves are load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the literal_hierarchical reading of kernel quranic_gender_verses; sibling readings contextual_egalitarian and progressive_abrogation instantiate structurally different constraints from the same verses - what exactly would adopting a sibling change?',
    'Comparative compilation of the sibling stories: adopt the egalitarian reading and women exit the victim set while the juristic extraction advantage dissolves (epsilon falls sharply); adopt the abrogation reading and the verses become a transitional stage with sunset character. The disagreement is located entirely in the operative status of the rulings (direct-timeless-binding vs context-mediated vs superseded), not in the text or its history.',
    'Sibling adoption would move this constraint''s victim set to empty, collapse the jurist seat''s agenda-setter position, and drop effective extraction toward coordination-cost levels; the literal reading''s high epsilon is a property of THIS reading, not of the verses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading contested kernel; sibling adoption changes victim sets, beneficiary structure, and epsilon.').

omega_variable(
    maintenance_compensation_offset,
    'Does the male obligation bundle (household maintenance, mahr, dowry payments) offset the resource transfer enough that net extraction is materially below the gross share differential?',
    'Household-level net-transfer accounting across representative samples in literal-application jurisdictions: compare lifetime inter-household flows against the inheritance and testimony differentials.',
    'If the offset is substantial and reliably discharged, epsilon drops toward symmetric and the coordination reading strengthens; if women''s guaranteed maintenance remains contingent on compliance while male shares are unconditional property, the asymmetry stands and epsilon holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_compensation_offset, empirical, 'Whether the compensation argument neutralizes the measured transfer asymmetry.').

omega_variable(
    timelessness_premise_stability,
    'Is the ''timeless direct ordinance'' premise itself stable, or does it rest on contested hermeneutic commitments (dismissal of occasion-of-revelation reports, restricted abrogation doctrine) that internal jurisprudential pressure could shift?',
    'Track how the reading''s own usul al-fiqh handles asbab al-nuzul evidence for these specific verses and whether intra-tradition scholarship narrows the anti-contextual commitments.',
    'If the timeless premise weakens internally, the constraint migrates toward the progressive_abrogation sibling''s structure and epsilon falls without any external actor forcing the change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timelessness_premise_stability, conceptual, 'Stability of the hermeneutic commitments that make the reading literal rather than contextual.').

omega_variable(
    jurisdictional_enforcement_variance,
    'Operation ranges from codified literal application in some states, through reformed personal-status codes (Morocco''s Mudawana, Tunisia''s family law), to customary dilution - is the measured epsilon one constraint or a family of jurisdictional instantiations?',
    'Decomposition test: if per-jurisdiction epsilon estimates diverge beyond the scope-scaling band, split this story into per-jurisdiction stories linked by network edges.',
    'Decomposition would produce high-epsilon literal-jurisdiction stories and low-epsilon reformed-jurisdiction stories, changing aggregate corpus classification; the single-story form averages over variance the engine cannot see.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_enforcement_variance, empirical, 'Whether cross-jurisdiction variance in application exceeds what one constraint story can honestly carry.').

omega_variable(
    identity_lock_composition,
    'Is the constrained exit of women under this arrangement primarily structural (legal disability, economic dependency, apostasy statute) or internalized (piety identity fusion making exit unthinkable even where barriers lift)?',
    'Post-exit suppression trajectory: study women who leave literal-application communities - if the felt constraint persists after barriers are removed, the internalized component is substantial.',
    'If internalized, effective suppression exceeds the structural measure and persists across migration and legal reform; the constraint travels with its targets, raising the floor under any jurisdictional reform scenario.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_composition, empirical, 'Structural versus internalized composition of the suppression bearing on women''s exit.').

omega_variable(
    authority_grounding_framing,
    'Is the juristic establishment''s authority grounded in transmission fidelity (lineage: isnad chains, communal reception) or in the benefits of preventing kernel revision (extraction: the office exists only while the verses stay directly operative)?',
    'Signals cut both ways: the tradition''s self-presentation (authenticated transmission, tawatur claims) supports lineage; the structural position (interpretive office, adjudication income, and gatekeeping power all depend on the text''s continued legal operativity) supports extraction. Comparative analysis of how the establishment responds to revision proposals - argumentative engagement versus sanction - would discriminate.',
    'An extraction-grounding classification would recast the interpreter layer as capture-shaped, attributing additional effective extraction to the interpretive apparatus itself rather than treating it as neutral transmission; the declared lineage framing keeps the interpreter layer analytically distinct from the extraction it administers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Under-determination between lineage and extraction framings of the authority structure; the lineage framing was chosen on the tradition''s self-presentation, the extraction framing on its structural position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 650, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t650, quranic_gender_verses__literal_hierarchical, theater_ratio, 650, 0.08).
narrative_ontology:measurement(qura_tr_t850, quranic_gender_verses__literal_hierarchical, theater_ratio, 850, 0.1).
narrative_ontology:measurement(qura_tr_t1000, quranic_gender_verses__literal_hierarchical, theater_ratio, 1000, 0.12).
narrative_ontology:measurement(qura_tr_t1258, quranic_gender_verses__literal_hierarchical, theater_ratio, 1258, 0.13).
narrative_ontology:measurement(qura_tr_t1550, quranic_gender_verses__literal_hierarchical, theater_ratio, 1550, 0.15).
narrative_ontology:measurement(qura_tr_t1850, quranic_gender_verses__literal_hierarchical, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(qura_tr_t1950, quranic_gender_verses__literal_hierarchical, theater_ratio, 1950, 0.24).
narrative_ontology:measurement(qura_tr_t2000, quranic_gender_verses__literal_hierarchical, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(qura_tr_t2025, quranic_gender_verses__literal_hierarchical, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t650, quranic_gender_verses__literal_hierarchical, base_extractiveness, 650, 0.62).
narrative_ontology:measurement(qura_be_t850, quranic_gender_verses__literal_hierarchical, base_extractiveness, 850, 0.7).
narrative_ontology:measurement(qura_be_t1000, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1000, 0.72).
narrative_ontology:measurement(qura_be_t1258, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1258, 0.73).
narrative_ontology:measurement(qura_be_t1550, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1550, 0.74).
narrative_ontology:measurement(qura_be_t1850, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1850, 0.71).
narrative_ontology:measurement(qura_be_t1950, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1950, 0.66).
narrative_ontology:measurement(qura_be_t2000, quranic_gender_verses__literal_hierarchical, base_extractiveness, 2000, 0.69).
narrative_ontology:measurement(qura_be_t2025, quranic_gender_verses__literal_hierarchical, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t650, quranic_gender_verses__literal_hierarchical, suppression_requirement, 650, 0.45).
narrative_ontology:measurement(qura_su_t850, quranic_gender_verses__literal_hierarchical, suppression_requirement, 850, 0.58).
narrative_ontology:measurement(qura_su_t1000, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1000, 0.63).
narrative_ontology:measurement(qura_su_t1258, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1258, 0.64).
narrative_ontology:measurement(qura_su_t1550, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1550, 0.66).
narrative_ontology:measurement(qura_su_t1850, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1850, 0.58).
narrative_ontology:measurement(qura_su_t1950, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1950, 0.62).
narrative_ontology:measurement(qura_su_t2000, quranic_gender_verses__literal_hierarchical, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(qura_su_t2025, quranic_gender_verses__literal_hierarchical, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, resource_allocation).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Qur'anic gender verses' covers three structurally distinct constraints that share a text but differ in the operative status of its rulings. This file (literal_hierarchical) is the upstream establishment position: the verses as directly binding timeless law, high epsilon, women in the victim set, jurists holding agenda control. The contextual_egalitarian sibling reads the same verses as historically situated steps requiring maqasid-mediated reinterpretation (lower epsilon, victim set thinned); the progressive_abrogation sibling reads them as a superseded stage of an egalitarian trajectory (transitional character). The upstream reading influences the downstream siblings structurally - both exist as counter-readings to it, and its enforcement machinery shapes the conditions under which they may be voiced - which is why the edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

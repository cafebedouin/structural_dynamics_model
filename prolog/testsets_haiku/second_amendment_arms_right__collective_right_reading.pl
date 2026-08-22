% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment as State Militia Right (Collective Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the collective-right reading of the
 *   Second Amendment: the right protects state militia authority, not
 *   individual ownership outside organized militia context. Under this
 *   reading, the prefatory militia clause ('a well regulated Militia, being
 *   necessary to the security of a free State') limits the operative clause
 *   ('the right of the people to keep and bear Arms, shall not be
 *   infringed'). The protected right is the state's capacity to arm and
 *   organize militia; individual civilians lack a Second Amendment bar to
 *   regulation. This reading was dominant in American law through much of the
 *   20th century but has been challenged and substantially superseded by the
 *   individual-right reading (Heller, 2008). The constraint story models the
 *   collective-right reading as it was authoritatively held—a genuine
 *   interpretation of the constitutional text with identifiable beneficiaries
 *   (state governments, regulatory authorities) and excluded voices
 *   (individual-right advocates, originalists). Low extractiveness reflects
 *   that this reading protects legitimate state governance functions (militia
 *   organization, public order) rather than coercive rent-seeking; but
 *   suppression is real because the reading's viability depends on excluding
 *   and delegitimizing the individual-right interpretation in public
 *   discourse and judicial doctrine. Theater ratio is moderate-to-declining:
 *   the reading once commanded genuine constitutional authority, but the
 *   decline in its doctrinal status (post-Heller) means maintaining it now
 *   requires increasingly defensive scholarly and interpretive work rather
 *   than the living authority it once held.
 *
 * KEY AGENTS:
 *   - State governments: institutional beneficiaries holding the protected militia right; agenda-setters in militia organization and civilian arms regulation
 *   - Federal government: constrained from disarming state militias but not from regulating individual civilian arms ownership
 *   - Individual firearm owners: excluded from Second Amendment protection on this reading; subject to comprehensive state and federal regulation
 *   - Collective-right legal scholars: organizing around the reading's preservation and doctrinal defense in the post-Heller landscape
 *   - Individual-right scholars and advocates: excluded from this reading's evidentiary framework; the reading's internal logic denies their core constitutional claim
 *   - Regulatory authorities: beneficiaries of a constitutional reading that permits comprehensive firearms regulation
 *   - Originalist interpreters: excluded from this reading's genealogy, which privileges post-ratification state practice over founding-era meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.22).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment as State Militia Right (Collective Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'd07f7e8b-b4ab-45ef-b23d-d50c589ac490').
narrative_ontology:cs_kernel_codification('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', fixed_text).
narrative_ontology:cs_authority_grounding('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', lineage).
narrative_ontology:cs_interpretation_layer_present('d07f7e8b-b4ab-45ef-b23d-d50c589ac490').
narrative_ontology:cs_reading_relation('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', foundational, militia_clause_is_limiting).
narrative_ontology:cs_axiom_status(militia_clause_is_limiting, holdable).
narrative_ontology:cs_axiom_grounding('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', militia_clause_is_limiting, empirically_contingent).
narrative_ontology:cs_axiom('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', secondary, state_governments_are_rights_holders).
narrative_ontology:cs_axiom_status(state_governments_are_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', state_governments_are_rights_holders, conventional).
narrative_ontology:cs_reference_frame('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', state_militia_protection_framework).
narrative_ontology:cs_drift_state('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', post_heller_doctrine, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d07f7e8b-b4ab-45ef-b23d-d50c589ac490', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, public_order_preservation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, legal_scholars_collective_reading).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, regulatory_authorities).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, gun_rights_advocates).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, well_regulated_militia_necessity_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_police_power_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State governments hold the right under this reading: to organize, train, and arm militia forces. They set firearms regulations, control militia structures (National Guard), and benefit from a constitutional framework that treats arms as a state prerogative. They administer the constraint and adjudicate its scope within state law.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter).

% The federal government is constrained from disarming state militias under this reading, but retains authority over federal firearms policy and individual civilian ownership outside militia contexts. The constraint limits federal militia-disarmament authority but does not expand federal regulatory authority over individual arms. The constraint is a modest constitutional bar, not a substantial extraction mechanism.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Individual civilians have no Second Amendment protection on this reading. They are subject to comprehensive state and federal firearms regulation without constitutional bar. They are excluded from the conversation about their own interests—the reading's logic treats their claims as outside the amendment's scope. This is the primary site of contestation: individual owners argue they are wrongly excluded; the collective-right reading excludes them by its core premise.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_firearm_owners, excluded,
    moderate, biographical, constrained, national).

% Scholars defending the collective-right reading (a minority position post-Heller but historically dominant in early-to-mid 20th century America) benefit from a textual interpretation they can defend in academic and legal forums. They research, publish, and litigate to preserve the reading's credibility. Their work maintains the reading's live-ness in legal discourse.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, legal_scholars_collective_reading, beneficiary,
    organized, biographical, mobile, national).

% Scholars of the individual-right reading (dominant post-Heller; includes DC v. Heller majority opinion) are excluded from this reading's own framework. The collective reading denies their core textual claim by treating the militia clause as limiting rather than explanatory. They argue the reading misreads founding-era meaning and distorts the operative clause's scope.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_right_scholars, excluded,
    organized, biographical, mobile, national).

% Federal and state law enforcement, public health agencies, and regulatory bodies benefit from a constitutional reading that permits comprehensive firearms regulation (licensing, background checks, ownership restrictions) without constitutional bar. They regulate without facing a Second Amendment individual-liberty defense. Under the individual-right reading, they must defend such regulations against constitutional challenges; under this reading, no such challenge is cognizable.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, regulatory_authorities, beneficiary,
    institutional, generational, analytical, national).

% Organizations and individuals claiming individual constitutional arms protections bear the cost of this reading: their constitutional claim is denied by the reading's own logic. They must argue against the reading in courts and public forums; they cannot rely on constitutional vindication and must operate through statutory exemptions and political contestation. Their organizational energy is consumed by fighting for doctrinal recognition.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates, payer,
    organized, biographical, constrained, national).

% Originalist constitutional scholars argue this reading ignores the founding-era understanding of 'the people,' 'bear arms,' and the militia clause's actual grammatical and functional role. They claim the reading was not the original meaning and was adopted primarily in 20th-century state supreme courts seeking regulatory latitude. They are excluded from this reading's evidentiary framework, which privileges post-ratification state practice and 20th-century precedent over original founding-era meaning.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, originalist_interpreters, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects state authority to organize, train, and arm militia forces without federal constitutional interference. Solves the governance problem of ensuring states retain the capacity to defend themselves and maintain internal order through armed citizen service — a coordination solution between state militia-building and federal power constraints.
% TRANSFER_FUNCTION: Transfers constitutional authority over civilian arms regulation from the individual-rights domain to the state-governmental domain. Moves the legal burden of proof: individual owners must argue for constitutional protection of ownership (a difficult claim under this reading), rather than regulators arguing why they can restrict a protected liberty.
% ABSENT_VOICES: Individual firearm owners who claim constitutional protection outside militia service are structurally excluded by this reading's definition of the right. Gun-rights scholars and originalists are excluded from the evidentiary framework, which privileges 20th-century state practice and post-ratification doctrinal development over original founding-era meaning. Second Amendment scholars of the individual-right tradition would object that the reading misreads the operative clause and distorts historical sources.
% DISAPPEARANCE_RATIONALE: If this reading's constitutional authority vanished (e.g., the Supreme Court formally rejected it in doctrine), states would retain statutory authority to regulate firearms, but the constitutional structure would shift: individual owners would possess a recognized federal constitutional claim rather than relying on statutory exemptions and regulatory discretion. The world does not rearrange fundamentally, but the constitutional framing of authority over arms changes. The contest reflects disagreement about whether individual owners have pre-political rights or whether arms regulation is a matter of state discretion.
% FOUNDING_PROBLEM: The Second Amendment text needed interpretation to reconcile 'a well regulated Militia, being necessary to the security of a free State' (militia-protective language) with 'the right of the people to keep and bear Arms, shall not be infringed' (rights-protective language). The founding problem is: does the militia clause limit the right, or merely explain its purpose? Does 'the people' mean individuals or the political community organized as militia?
% FOUNDING_PROBLEM_CORROBORATION: Scholars on both sides of the contest (individual-right and collective-right traditions) agree the text is ambiguous and has been read different ways across American history. The collective-right reading cites early state supreme court decisions (e.g., Bliss v. Commonwealth, 1822) and the post-Reconstruction narrative that the amendment was not incorporated against states until the 20th century. Individual-right scholars (Heller era, post-2008 doctrine) argue early state practice included individual militia participation and the operative clause was not limited by the militia clause. No consensus source corroborates the founding problem as 'solved' — the contest persists in contemporary jurisprudence.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness value (0.15 at interval end, starting at 0.08) is low and stable because the constraint protects legitimate state governance functions—militia organization, public order, regulatory authority over arms—rather than asymmetric rent extraction. There is no identifiable victim group bearing concentrated costs; instead, regulatory authorities and state governments benefit from expanded discretion. The suppression value (0.22 at interval end, declining from 0.35) reflects the reading's declining doctrinal status: in mid-20th-century America, suppression was higher because the reading held authoritative force and alternative interpretations were marginalized in mainstream constitutional discourse. The decline in suppression over the interval models the erosion of the reading's live-ness in constitutional law post-Heller: it is no longer the default interpretive frame, and defending it requires increasingly active work against the dominant individual-right reading. Theater ratio mirrors this: early in the interval (t0), defending the collective-right reading required significant performative work to maintain its plausibility against rising individual-right scholarship; by t50, the reading is marginalized enough that theater costs have declined (fewer parties still hold it as a live interpretation). The temporal series models a reading in doctrinal decline, not an extraction mechanism that has strengthened.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is NOT between payer and beneficiary (there is no payer seat; state governments benefit cleanly). The divergence is between the reading's own internal frame and the excluded voices it denies. From the collective-right reading's own perspective, the constraint is pure coordination: states organize militia, individuals are subject to plenary regulation. From the excluded seats—individual-right scholars, gun-rights advocates, originalists—the reading is a misinterpretation that denies a pre-political constitutional right. This is a content disagreement, not a structural asymmetry between beneficiary and victim seats. The engine will compute the same type across all seats because the constraint has no extraction asymmetry; the disagreement is about whether the reading's core premise (militia clause limits the right) is valid.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is straightforward because there are no victims: state governments benefit from expanded regulatory authority (d near 0.0, full beneficiary). Individual owners are excluded rather than targeted; they occupy no stakeholder seat with a role within this reading's logic. Regulatory authorities benefit from constrained judicial review of firearms regulations (d near 0.0). Federal government is minimally constrained by the reading (d near 0.2, slight target status: the reading prevents federal disarmament of state militias, but this is a minor constraint given the modern National Guard's constitutional integration). No directionality override is needed; the structural derivation is clean.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT exhibit mandatrophy in the classical sense (a function that has been solved and the constraint should sunset). The founding problem—interpreting the militia clause—is live and contested; there is no consensus that the collective-right reading has become obsolete in principle. However, the reading does show what might be called 'doctrine-level obsolescence': the individual-right reading has become dominant in Supreme Court and mainstream legal scholarship, substantially displacing the collective-right reading from its formerly authoritative position. The mandatrophy question is whether the collective-right reading should persist as a live constitutional option or whether the individual-right reading has so decisively won the interpretive field that the collective reading is now merely a historical artifact. The theater_ratio decline over the interval models this: less performative work is needed to maintain the reading because fewer parties are trying to. Mandatrophy_resolved is false because the reading can still be advanced, defended, and litigated; but the reading's vitality is declining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_limiting_vs_explanatory,
    'Does the prefatory militia clause (''a well regulated Militia, being necessary to the security of a free State'') limit the operative clause to militia contexts, or is it merely explanatory of the operative clause''s purpose?',
    'Historical linguistic analysis (18th-century grammar and usage), examination of founding-era state constitutions and documents using similar prefatory structures, analysis of how such clauses functioned in contemporary legal texts.',
    'If limiting: this reading is structurally sound; the operative clause applies only to militia service. If explanatory: the operative clause protects individual arms ownership regardless of militia status, and this reading misreads the text. The foundational axiom (militia_clause_is_limiting) stands or falls on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_limiting_vs_explanatory, empirical, 'Whether the militia clause grammatically and functionally limits the operative clause.').

omega_variable(
    the_people_collective_vs_individual,
    'In the Second Amendment text, does ''the people'' refer to the political community (as in ''We, the people'') or to individuals in their capacity as citizens?',
    'Examination of 18th-century usage in founding-era documents (Declaration, Constitution, Federalist Papers, state documents), linguistic and legal analysis of the phrase across contemporary texts.',
    'If collective: the right is a state-level or collective right, supporting this reading. If individual: the right is held by individuals, supporting the individual-right reading. This is the deepest structural disagreement between the readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(the_people_collective_vs_individual, empirical, 'Whether ''the people'' in the Second Amendment denotes a collective or individual referent.').

omega_variable(
    incorporation_and_doctrinal_supersession,
    'Has the dominance of the individual-right reading in contemporary Supreme Court doctrine and mainstream legal scholarship effectively superseded the collective-right reading, or does the collective reading retain structural viability as a defensible constitutional interpretation?',
    'Monitoring Supreme Court doctrine (whether Heller and post-Heller cases foreclose the collective reading or merely reject it as less plausible); tracking appellate decisions and legal scholarship; assessing whether lower courts or state courts continue to cite collective-right interpretations as live options.',
    'If superseded: this reading becomes a historical artifact rather than a live interpretation; its doctrinal authority is gone. If viable: it remains a minority but defensible reading in constitutional discourse. Current trajectory (post-2008) is toward supersession, but the reading is not formally foreclosed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incorporation_and_doctrinal_supersession, conceptual, 'Whether the collective-right reading has been doctrinal superseded or remains a live constitutional option.').

omega_variable(
    state_militia_vs_national_guard,
    'On the collective-right reading, does the militia right protect state authority to organize citizen militia independent of federal control, or does the incorporation of state militias into the National Guard satisfy the militia-protection function?',
    'Examining whether the collective-right reading''s proponents argue the National Guard (federally integrated) fulfills the militia purpose or whether they argue for a separate state militia independent of federal command structure.',
    'If National Guard satisfies the function: the reading''s rationale may be obsolete (states have federally-integrated militia forces). If independent militia is required: the reading implies a structural problem with modern National Guard integration, which few modern courts accept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_militia_vs_national_guard, empirical, 'Whether the modern National Guard structure fulfills the collective-right reading''s militia-protection function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(seco_tr_t0, projected).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__collective_right_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__collective_right_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__collective_right_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(seco_tr_t30, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__collective_right_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_arms_right__collective_right_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(seco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(seco_be_t0, projected).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__collective_right_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__collective_right_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__collective_right_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement_basis(seco_be_t30, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__collective_right_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_arms_right__collective_right_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement_basis(seco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(seco_su_t0, projected).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__collective_right_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__collective_right_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__collective_right_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement_basis(seco_su_t30, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__collective_right_reading, suppression_requirement, 40, 0.23).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_arms_right__collective_right_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(seco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel (second_amendment_arms_right) instantiates three structurally distinct constraints corresponding to three live readings: collective_right_reading (this story), individual_right_reading, and civic_republican_reading. Each reading produces a different constraint with different ε, different beneficiary structures, and different legal implications. The three stories are linked via network.affects_constraints because they compete for interpretive authority over the same text. The individual_right_reading superseded this collective_right_reading in mainstream Supreme Court doctrine post-Heller (2008), but all three readings remain live in legal scholarship and judicial debate. Each story must be authored separately with its own ε value (the collective reading sees low ε for legitimate militia-protection functions; the individual reading sees higher ε for the individual liberty claim; the civic reading produces a middle position). Do not merge the three readings into one constraint—they are incommensurable framings of a single kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

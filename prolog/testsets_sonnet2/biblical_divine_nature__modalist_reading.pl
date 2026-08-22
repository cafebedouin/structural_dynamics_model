% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__modalist_reading, []).

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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Reading of Divine Nature (Father/Son/Spirit as Sequential Modes)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint captures the modalist (Sabellian/Patripassian) reading of
 *   the Father-Son-Spirit relation: one divine person acting successively in
 *   three modes rather than three eternally co-existing persons. Early in the
 *   interval (c. 190 CE, associated with Noetus and later Sabellius) it
 *   functioned as a low-suppression, low-enforcement theological option
 *   circulating among communities seeking a monotheism-preserving account of
 *   Christ's full divinity without inventing new philosophical vocabulary. As
 *   trinitarian conciliar authority consolidated across the third and early
 *   fourth centuries, enforcement against modalism intensified sharply —
 *   condemnations, depositions, and eventually formal heresy designation —
 *   while the reading itself persisted institutionally chiefly among
 *   communities unwilling or unable to adopt the hypostasis/ousia
 *   distinction. This is the modalist_reading constraint ONLY: the
 *   trinitarian_reading and unitarian_reading are separate sibling
 *   constraints in the same kernel contest and are not evaluated here.
 *
 * KEY AGENTS:
 *   - modalist_clergy: agenda-setters who articulate and administer the reading, later bearing institutional risk
 *   - jesus_centered_devotional_communities: beneficiaries of devotional simplicity
 *   - modalist_laity_under_condemnation: powerless payers who inherit the doctrine and bear its later condemnation
 *   - excommunicated_sabellian_teachers: direct targets of conciliar suppression
 *   - trinitarian_episcopal_authorities: excluded institutional authority whose rulings determine the payers' fate
 *   - unitarian_theological_critics: excluded rival reading's proponents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.42).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.55).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Reading of Divine Nature (Father/Son/Spirit as Sequential Modes)").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '7fca99dc-f59a-43ca-953a-390beeba8231').
narrative_ontology:cs_kernel_codification('7fca99dc-f59a-43ca-953a-390beeba8231', distributed).
narrative_ontology:cs_authority_grounding('7fca99dc-f59a-43ca-953a-390beeba8231', distributed).
narrative_ontology:cs_reading_relation('7fca99dc-f59a-43ca-953a-390beeba8231', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('7fca99dc-f59a-43ca-953a-390beeba8231', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_axiom('7fca99dc-f59a-43ca-953a-390beeba8231', foundational, single_person_sequential_self_revelation).
narrative_ontology:cs_axiom_status(single_person_sequential_self_revelation, holdable).
narrative_ontology:cs_axiom_grounding('7fca99dc-f59a-43ca-953a-390beeba8231', single_person_sequential_self_revelation, conventional).
narrative_ontology:cs_axiom('7fca99dc-f59a-43ca-953a-390beeba8231', foundational, monotheism_requires_numerical_identity_of_agent).
narrative_ontology:cs_axiom_status(monotheism_requires_numerical_identity_of_agent, overridden).
narrative_ontology:cs_axiom_grounding('7fca99dc-f59a-43ca-953a-390beeba8231', monotheism_requires_numerical_identity_of_agent, deontological).
narrative_ontology:cs_reference_frame('7fca99dc-f59a-43ca-953a-390beeba8231', pre_nicene_undifferentiated_monotheism).
narrative_ontology:cs_drift_state('7fca99dc-f59a-43ca-953a-390beeba8231', post_nicene_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7fca99dc-f59a-43ca-953a-390beeba8231', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, modalist_clergy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, jesus_centered_devotional_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, modalist_laity_under_condemnation).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, excommunicated_sabellian_teachers).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, strict_monotheism_priority).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, economy_of_salvation_simplicity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and administer the modalist reading within their congregations, framing Father, Son, and Spirit as successive self-revelations of one divine person. They gain doctrinal authority and pastoral simplicity from a reading that avoids the philosophical apparatus of hypostatic distinction, but face escalating condemnation from wider church councils, which threatens their institutional standing and communion with larger bodies.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_clergy, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, modalist_clergy, beneficiary).

% Worship in a framework where the God who suffered on the cross is directly and unambiguously the same God who is Father — no mediating persons complicate the devotional immediacy. They benefit from theological simplicity but bear the risk of being labeled heretical by surrounding orthodox communities, which can mean social and ecclesial isolation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, jesus_centered_devotional_communities, beneficiary,
    moderate, biographical, constrained, local).

% Ordinary believers formed in modalist teaching who are later excommunicated, denied sacraments, or socially ostracized once councils rule the position heretical. They did not design the doctrine; they inherited it through their local teachers and now bear the cost of a controversy fought at the institutional level, with little recourse to appeal or exit given the social embeddedness of their religious community.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_laity_under_condemnation, payer,
    powerless, biographical, trapped, local).

% Teachers (historically associated with Sabellius and Noetus) who articulated the modalist position and were formally condemned, deposed, and in some cases exiled by episcopal authorities. They bear direct institutional punishment for maintaining the reading once it was ruled outside acceptable bounds, with no meaningful path to continue teaching within the sanctioned church structure.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, excommunicated_sabellian_teachers, payer,
    moderate, biographical, trapped, regional).

% Bishops and councils (e.g., those opposing Sabellius, Noetus, Praxeas) who would object strenuously to this reading as collapsing the distinct persons into a single actor performing roles, seeing it as undermining the reality of the Father-Son relationship attested in scripture (e.g., Jesus praying to the Father). They are not stakeholders inside this constraint's operation but are the excluding authority whose rulings shape the payer seats' fate — named here as excluded from the modalist reading's own internal frame, not absent from history.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_episcopal_authorities, excluded,
    institutional, civilizational, analytical, continental).

% Those holding that only the Father is fully and numerically God would object that modalism, despite claiming strict monotheism, still grants the Son and Spirit modes of full divine identity and worship, which they see as functionally indistinguishable from polytheistic accretion. Excluded from the modalist reading's internal justification, which treats their subordinationist alternative as a separate error.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, unitarian_theological_critics, excluded,
    organized, generational, analytical, regional).

% Study the modalist controversy as a case of contested kernel-reading within early Christian commitment to monotheism, tracing how councils, bishops, and communities negotiated which reading would carry institutional legitimacy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, later_doctrinal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__modalist_reading, modalist_clergy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__modalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Modalism solves the real problem of reconciling strict monotheism (one God, no division) with the scriptural attestation that Father, Son, and Spirit are each called God and each act divinely — by proposing they are one person acting in sequential roles/modes rather than three eternally distinct persons. It gives ordinary believers a devotionally simple answer: the God who saves is the same single God as the Father who sent him.
% TRANSFER_FUNCTION: The reading moves theological and institutional authority away from communities that would develop a hypostatic/relational vocabulary (which modalism treats as unnecessary complexity) and concentrates authority in the sequential-mode framing held by the teachers who articulate it. Once this framing was later ruled heretical, the transfer reversed: the cost of the earlier grant of legitimacy fell onto adherents who were excommunicated and had communion, social standing, and often clerical office stripped from them.
% ABSENT_VOICES: Trinitarian episcopal authorities and unitarian theological critics are structurally excluded from modalism's own internal frame — the reading does not engage their strongest objections (the eternal Father-Son relational language in scripture, or the concern that modalism still deifies more than one numerically distinct entity in practice) except to dismiss them. Both would object vigorously if given equal footing in this constraint's internal justification.
% DISAPPEARANCE_RATIONALE: If the modalist reading vanished as a live commitment, the specific devotional communities organized around it would lose their theological self-description and would either dissolve, reform around a different Christological framework, or be absorbed into trinitarian or unitarian bodies; the historical record of condemnations, depositions, and excommunications tied to this reading would also lose their object, since there would be no modalist teaching left to condemn.
% FOUNDING_PROBLEM: Early Christian communities needed to affirm both that Jesus Christ is fully God and that God is absolutely one, without a developed philosophical vocabulary (ousia/hypostasis) to express how three could be one without being three gods. Modalism solved this by denying real simultaneous distinction: one God, three successive self-presentations.
% FOUNDING_PROBLEM_CORROBORATION: Trinitarian episcopal authorities (via conciliar condemnations of Sabellius, Noetus, and Praxeas, and later councils affirming hypostatic distinction) attest from outside the modalist community that the vocabulary problem was subsequently resolved through the ousia/hypostasis distinction, making the modalist solution unnecessary within the framework the wider church adopted. No corroboration from a neutral non-theological source exists; all surviving attestation comes from either modalism's own defenders or the councils that condemned it, which is itself the contested ground the omega below documents.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).
:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises moderately (0.20 to 0.42) over the interval as the reading moves from an unremarkable theological option to a contested and eventually condemned position — the 'extraction' here is institutional: modalist communities and teachers invest identity, teaching authority, and communal standing in the reading, then lose standing, office, and communion once wider ecclesial authority rules against it. Suppression rises more sharply (0.15 to 0.55) tracking the intensifying conciliar and episcopal enforcement against Sabellianism specifically, which is the dominant historical dynamic in this period. Theater ratio is moderate and rising (0.10 to 0.30): some of the later condemnations functioned partly as performative boundary-marking for orthodox identity formation rather than sustained pastoral engagement with modalist communities' actual concerns.
 *
 * DIRECTIONALITY LOGIC:
 *   Modalist clergy and jesus_centered_devotional_communities are declared beneficiaries because the reading gives them theological coherence and devotional immediacy without requiring philosophical apparatus — low-to-moderate directionality toward extraction. Modalist_laity_under_condemnation and excommunicated_sabellian_teachers are declared victims because they bear the concrete costs (excommunication, loss of office, social exile) of a doctrinal position whose institutional risk they did not choose to assume at the scale it eventually cost them — high directionality toward extraction, amplified by trapped exit options once local religious community is the primary social structure available to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling strict monotheism with full divine attribution to Christ absent a developed relational vocabulary — is genealogically real and was live circa 190-220 CE. By the time of Nicene and post-Nicene consolidation, the wider church had developed the hypostasis/ousia distinction that addressed the same underlying problem differently, rendering the modalist solution's specific founding problem largely resolved elsewhere (status: dead) while the reading persisted among communities that either rejected or had not adopted the new vocabulary. This is not classic mandatrophy (an extraction machine dressed as coordination) so much as a doctrinal position whose solved problem outlived its social carriers — the six_questions mismatch check (dead status + world_rearranges verdict) flags this as a genealogy worth scrutiny rather than settled fact, since corroboration comes only from the condemning side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modalist_reading_kernel_disambiguation,
    'Is this constraint properly evaluated as modalism''s own internal coordination/extraction structure, or does its ε implicitly smuggle in the trinitarian councils'' framing of what counts as ''heretical''?',
    'Compare independent modalist-community sources (where they survive, e.g. fragments preserved in hostile heresiological accounts vs. any surviving modalist-authored texts) against conciliar condemnation texts to separate modalism''s self-understanding from its opponents'' characterization.',
    'If ε here is contaminated by trinitarian framing, the extraction measured may overstate modalism''s own internal extractive structure and understate that the ''extraction'' is actually being imposed FROM OUTSIDE by the winning trinitarian reading — which would be more accurately captured as suppression exerted by a sibling constraint (trinitarian_reading) rather than as intrinsic to this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modalist_reading_kernel_disambiguation, conceptual, 'Whether ε is measuring modalism''s own structure or the trinitarian lens applied to it.').

omega_variable(
    sequential_mode_biblical_warrant_ambiguity,
    'Does the biblical text underdetermine between the modalist and trinitarian readings of passages like the baptism of Jesus (where Father, Son, and Spirit appear to act simultaneously and distinctly) or does it clearly favor one reading?',
    'Close textual-historical analysis of key passages (e.g. Matthew 3:16-17, John 17) alongside first- and second-century reception history, independent of later conciliar interpretive frameworks.',
    'If the text underdetermines, modalism''s exclusion is better understood as an interpretive-tradition choice (a founding-problem-status of ''contested'' rather than ''dead'') rather than a doctrinally settled matter; if the text clearly favors simultaneous distinction, the founding_problem_status of ''dead'' is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sequential_mode_biblical_warrant_ambiguity, empirical, 'Whether scripture itself adjudicates between sequential-mode and simultaneous-person readings.').

omega_variable(
    beneficiary_or_casualty_of_own_success,
    'Are modalist_clergy genuinely net beneficiaries of the reading, or does their apparent benefit collapse once the full trajectory (early adoption, later condemnation, loss of office) is taken into account?',
    'Track individual and community-level outcomes across the full interval rather than at a single time-slice; compare standing at t=190 to standing at t=320 for communities that held the reading throughout.',
    'If net outcomes are negative even for clergy, the beneficiary/payer split should be revised toward treating modalist_clergy as payers as well, which would shift this constraint''s classification consideration toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_or_casualty_of_own_success, empirical, 'Whether clergy benefit net-positively or are ultimately also casualties of holding the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 190, 320).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t190, biblical_divine_nature__modalist_reading, theater_ratio, 190, 0.1).
narrative_ontology:measurement(bibl_tr_t210, biblical_divine_nature__modalist_reading, theater_ratio, 210, 0.15).
narrative_ontology:measurement(bibl_tr_t230, biblical_divine_nature__modalist_reading, theater_ratio, 230, 0.2).
narrative_ontology:measurement(bibl_tr_t250, biblical_divine_nature__modalist_reading, theater_ratio, 250, 0.24).
narrative_ontology:measurement(bibl_tr_t270, biblical_divine_nature__modalist_reading, theater_ratio, 270, 0.27).
narrative_ontology:measurement(bibl_tr_t290, biblical_divine_nature__modalist_reading, theater_ratio, 290, 0.29).
narrative_ontology:measurement(bibl_tr_t320, biblical_divine_nature__modalist_reading, theater_ratio, 320, 0.3).

% Extraction over time
narrative_ontology:measurement(bibl_be_t190, biblical_divine_nature__modalist_reading, base_extractiveness, 190, 0.2).
narrative_ontology:measurement(bibl_be_t210, biblical_divine_nature__modalist_reading, base_extractiveness, 210, 0.28).
narrative_ontology:measurement(bibl_be_t230, biblical_divine_nature__modalist_reading, base_extractiveness, 230, 0.35).
narrative_ontology:measurement(bibl_be_t250, biblical_divine_nature__modalist_reading, base_extractiveness, 250, 0.38).
narrative_ontology:measurement(bibl_be_t270, biblical_divine_nature__modalist_reading, base_extractiveness, 270, 0.4).
narrative_ontology:measurement(bibl_be_t290, biblical_divine_nature__modalist_reading, base_extractiveness, 290, 0.41).
narrative_ontology:measurement(bibl_be_t320, biblical_divine_nature__modalist_reading, base_extractiveness, 320, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t190, biblical_divine_nature__modalist_reading, suppression_requirement, 190, 0.15).
narrative_ontology:measurement(bibl_su_t210, biblical_divine_nature__modalist_reading, suppression_requirement, 210, 0.25).
narrative_ontology:measurement(bibl_su_t230, biblical_divine_nature__modalist_reading, suppression_requirement, 230, 0.4).
narrative_ontology:measurement(bibl_su_t250, biblical_divine_nature__modalist_reading, suppression_requirement, 250, 0.48).
narrative_ontology:measurement(bibl_su_t270, biblical_divine_nature__modalist_reading, suppression_requirement, 270, 0.52).
narrative_ontology:measurement(bibl_su_t290, biblical_divine_nature__modalist_reading, suppression_requirement, 290, 0.54).
narrative_ontology:measurement(bibl_su_t320, biblical_divine_nature__modalist_reading, suppression_requirement, 320, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__modalist_reading, 0.1).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, unitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the biblical_divine_nature kernel. trinitarian_reading (three hypostases, one ousia) and unitarian_reading (numerical singularity, Father alone fully God) are separate stories with independently authored ε, stakeholders, and classification. The modalist_reading's core premise (one person, sequential modes, no simultaneous distinction) directly forecloses the trinitarian_reading's core premise (three simultaneously subsisting persons) within any single coherent framework — a genuine forecloses relation, the rarer of the three relation types. It coexists_with the unitarian_reading because both readings can be held as live minority or majority positions by different communities without one logically requiring the rejection of the other's framework (both reject Nicene trinitarianism from different directions, but do not contradict each other directly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

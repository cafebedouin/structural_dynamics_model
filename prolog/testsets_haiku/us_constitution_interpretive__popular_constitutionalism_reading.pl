% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Constitutional Meaning as Democratic Contestation
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   The popular constitutionalism reading claims that constitutional meaning
 *   emerges from democratic contestation and popular movements, not from
 *   judicial pronouncements alone. Under this reading, 'the people' retain
 *   ongoing constituent power; legislatures and executives can act on their
 *   own constitutional judgment; and popular mobilization is a legitimate
 *   (indeed, necessary) source of constitutional evolution. The constraint
 *   maps the structural consequences of this claim: if interpretive authority
 *   is genuinely shared, then majorities and movements gain power
 *   (beneficiaries) while those dependent on counter-majoritarian judicial
 *   finality lose security (victims). The reading does not deny that courts
 *   interpret the Constitution—it denies that they do so with exclusive,
 *   final authority. This creates a tangled coordination structure: genuine
 *   democratic contestation over constitutional meaning (coordination
 *   function) rides alongside extraction from powerless minorities who depend
 *   on judicial protection (victims). The constraint is CLAIMED as
 *   tangled_rope because it coordi­nates a shared framework for
 *   constitutional contest while asymmetrically extracting security from
 *   those who need counter-majoritarian judicial gatekeeping.
 *
 * KEY AGENTS:
 *   - Popular movements (civil rights, labor, suffrage, environmental): framing constitutional claims as deriving from sovereign people, not judicial license
 *   - Legislative majorities: claiming independent constitutional authority and enacting laws on their own constitutional reading
 *   - Executive branches: asserting constitutional duty to construe provisions in executing their office, independent of settled doctrine
 *   - Judicial finality advocates: institutional lawyers and judges defending court monopoly on final constitutional interpretation
 *   - Constitutional minorities (religious, racial, sexual-orientation, unpopular speakers): dependent on counter-majoritarian judicial protection
 *   - Anti-elitist constituencies: viewing the judiciary as illegitimate gatekeeper; benefiting from democratized interpretive authority
 *   - Constitutional scholars: documenting historical practice of constitutional change outside and before courts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.52).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism: Constitutional Meaning as Democratic Contestation").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'efcb50d9-aa61-4061-8957-74802da3ff72').
narrative_ontology:cs_kernel_codification('efcb50d9-aa61-4061-8957-74802da3ff72', fixed_text).
narrative_ontology:cs_authority_grounding('efcb50d9-aa61-4061-8957-74802da3ff72', distributed).
narrative_ontology:cs_reading_relation('efcb50d9-aa61-4061-8957-74802da3ff72', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('efcb50d9-aa61-4061-8957-74802da3ff72', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('efcb50d9-aa61-4061-8957-74802da3ff72', foundational, popular_sovereignty_ongoing).
narrative_ontology:cs_axiom_status(popular_sovereignty_ongoing, holdable).
narrative_ontology:cs_axiom_grounding('efcb50d9-aa61-4061-8957-74802da3ff72', popular_sovereignty_ongoing, deontological).
narrative_ontology:cs_axiom('efcb50d9-aa61-4061-8957-74802da3ff72', foundational, constitutional_interpretation_democratic_authority).
narrative_ontology:cs_axiom_status(constitutional_interpretation_democratic_authority, holdable).
narrative_ontology:cs_axiom_grounding('efcb50d9-aa61-4061-8957-74802da3ff72', constitutional_interpretation_democratic_authority, instrumental).
narrative_ontology:cs_reference_frame('efcb50d9-aa61-4061-8957-74802da3ff72', popular_constituent_authority).
narrative_ontology:cs_drift_state('efcb50d9-aa61-4061-8957-74802da3ff72', contemporary_judicial_supremacy_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('efcb50d9-aa61-4061-8957-74802da3ff72', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_constituencies).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_minorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_dependent_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, executive_branches).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_judicial_protection_dependents).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, democratic_contestation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social, political, and reform movements (civil rights, labor, suffrage, environmental, LGBTQ+, immigrant rights) claim constitutional meaning directly through mass mobilization, legislative pressure, and street-level defiance of inherited judicial interpretations. They frame constitutional change as belonging to 'the people' acting in their sovereign capacity, not to judges. Their wins are constitutional amendments, legislative victories that redefine practice, and shifts in what is politically enforceable even absent formal judicial reversal. They gain interpretive authority and standing to shape constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, biographical, mobile, national).

% Congress and state legislatures claim interpretive authority to act on their own constitutional judgment, not subordinate to final judicial rulings. Under this reading, legislatures enact laws they believe constitutional despite Supreme Court precedent in a contrary direction, triggering fresh popular and judicial contest. They are the seat where the democratic will most directly expresses itself and where constitutional change happens before (and sometimes instead of) the courts recognize it. Gains power to set constitutional meaning through legislation and to override judicial finality.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, agenda_setter,
    institutional, biographical, analytical, national).

% Presidents and their administrations claim independent constitutional duty to construe the Constitution in executing their office, sometimes in tension with settled judicial doctrine. They face pressure both to honor counter-majoritarian judicial protections and to respond to electoral mandates. Their position is dual: they set constitutional meaning through executive action and face the cost of sustained judicial or popular pushback if their reading diverges too far from stable consensus. Gains authority to interpret but faces constraint of legitimacy and political contest.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, executive_branches, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, executive_branches, payer).

% Jurists, legal scholars, and institutional actors invested in judicial supremacy and the stability of established constitutional doctrine. They argue that constitutional meaning must be settled by courts, not subject to continuous popular revision, because stability and rule-of-law require finality. They bear the cost of legitimacy erosion as popular movements disregard or openly contest judicial pronouncements; they also bear institutional costs when legislatures ignore precedent or executive actors act on alternative constitutional readings. Loses institutional monopoly on constitutional authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Groups whose rights depend on counter-majoritarian judicial protection: religious minorities, racial minorities with restricted political power, unpopular political dissidents, LGBTQ+ persons in majoritarian jurisdictions, immigrant communities, indigenous peoples. Under popular constitutionalism, their protection is contingent on popular movements adopting their cause or on the political momentum of the moment. They lose the recourse to courts as final arbiters of their rights and gain exposure to majoritarian constitutional reinterpretation. Their exit is limited: they cannot leave the polity's constitutional order except by migration. Bears the cost of losing judicial protection.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_minorities, payer,
    powerless, biographical, trapped, national).

% A broader category than discrete minorities: persons or groups whose liberties or claims depend on judicial enforcement against legislative majorities or popular sentiment. They include incarcerated persons, religious dissenters, unpopular speakers, non-citizens, and those with privacy claims at odds with majoritarian preferences. Under popular constitutionalism, their security is weakened because 'the people' as a political force may vote to limit or eliminate their protections, and judges are no longer positioned as final defenders of constitutional limits on popular will. Bears diffuse cost of exposure to majoritarian constitutional revision.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_judicial_protection_dependents, payer,
    powerless, biographical, trapped, national).

% Citizens and constituencies that perceive the judiciary (especially the Supreme Court) as an elite, insulated body unaccountable to popular will and therefore illegitimate as the final arbiter of constitutional meaning. Popular constitutionalism appeals to them because it repositions constitutional authority in democratic processes and popular struggle, reducing judicial gatekeeping power over constitutional claims. They benefit from a reading that democratizes interpretive authority and gives them voice in shaping constitutional meaning. Gains legitimacy and power through democratized authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_constituencies, beneficiary,
    moderate, biographical, mobile, national).

% Legal theorists and judges who argue that only courts can authoritively interpret the Constitution and that coordinate-branch and popular claims to constitutional interpretation are illegitimate or incoherent. They would argue that popular constitutionalism dissolves constitutional meaning into mere politics and destroys the rule of law. Their exclusion from the shared-authority frame is the reading's core move; they are not represented in the contest for constitutional meaning as a legitimate voice. Structurally excluded from authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacists, excluded,
    institutional, generational, constrained, national).

% Academic interpreters and historians who study how constitutional meaning has actually formed in practice: through legislative action, popular mobilization, executive claims, and shifts in what is politically enforceable. They observe that constitutional change (suffrage, federalism power shifts, civil rights) has often preceded or bypassed judicial pronouncements. Their analytical seat provides evidence about whether popular constitutionalism describes actual American constitutional practice or prescribes a counter-historical ideal. Provides analytical observation without direct stake.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared framework for claiming and contesting constitutional meaning: popular movements, legislatures, and executives can invoke constitutional authority without seeking prior judicial license, creating multiple sites of constitutional contestation and evolution rather than concentrating authority in courts.
% TRANSFER_FUNCTION: Transfers interpretive authority (and the legitimacy it confers) from courts to popular movements and democratic branches. Movements gain standing to reshape constitutional meaning through mass mobilization and political pressure rather than litigation. Courts lose monopoly on constitutional pronouncement. Majorities gain power to enact their constitutional vision without final judicial veto.
% ABSENT_VOICES: Judicial supremacists and constitutional fundamentalists who hold that only courts can legitimately interpret the Constitution are structurally excluded by the reading's core move. Those dependent on stable, entrenched judicial constitutional protections (minorities, dissenters, the powerless) are present in the frame but as payers rather than beneficiaries—their voices are heard but their interests are subordinated to popular will.
% DISAPPEARANCE_RATIONALE: Under the popular constitutionalism reading, if the norm of judicial finality suddenly held absolute force (the opposite state), popular movements would lose a powerful lever for constitutional change and would be forced to seek formal amendments or subordinate themselves to courts. Movements would reorganize around litigation and amendment, not mobilization. Legislatures would cease claiming independent constitutional judgment. The reading itself would become unintelligible—the constraint is the contestation, so its disappearance is the imposition of the rival (judicial supremacy) reading. Originalists and living constitutionalists might say constitutional meaning would be restored to stable ground; popular constitutionalists would say it would be illegitimately frozen.
% FOUNDING_PROBLEM: The need for constitutional meaning to evolve and for popular sovereignty to have real force in governance: the founding problem is that courts alone cannot generate the adaptations and democratic legitimacy needed for a constitution to remain alive and responsive to a changing people. Without popular contestation and legislative re-interpretation, constitutionalism becomes a tool of an elite judiciary insulated from democratic will.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians (Akhil Amar, popular constitutionalism scholars) and movement historians document that major constitutional changes (civil rights, voting rights, labor rights) preceded or occurred outside judicial pronouncements, driven by popular mobilization. Legal realists and critical scholars argue the founding problem is real: elite judicial gatekeeping produces unstable, delegitimized constitutional settlements. Judicial supremacists and rule-of-law advocates contest this, arguing the founding problem is a myth—courts do respond to long-term popular consensus and that judicial finality prevents majoritarian tyranny. The corroboration is split: historians of practice support the popular reading; institutional lawyers defending stability contest it.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, contested).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the reading transfers interpretive authority from courts to majoritarian sites (legislatures, movements) and thereby reduces the security of minorities who depend on judicial finality. The extraction is not from direct material transfer but from exposure: minorities' constitutional protections become contingent on majoritarian favor rather than judicially entrenched. Suppression is lower (0.52) because the reading is not internally enforced by active coercion—it relies on normative claims about democratic legitimacy and popular sovereignty. The constraint persists through political contestation and shifts in power, not through suppressive machinery. Theater is moderate (0.41): part of the reading's operation involves performative assertions of 'the people's' constitutional authority in mass mobilization and legislative debate, but the actual constitutional change is real—precedent is actually challenged, laws are actually enacted on alternative readings, movements actually shift what is politically enforceable. The measurement series shows extractiveness and suppression rising over the interval (0 to 40): as the popular constitutionalism reading gains intellectual and political currency (roughly 1990s onward), it generates increasing pressure on the judicial finality regime and increases the exposure of minorities dependent on that regime. Theater rises sharply early (0–15) as movements perform popular sovereignty, then plateaus as the reading matures and the performative element stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (legislatures, movements) and the payer (minorities, finality advocates) should compute divergent types from the structural data. From the legislative/movement seat, this is a genuine coordination frame for democratic contestation of constitutional meaning—a rope. From the minoritized seat, it is a constraint that exposes their rights to majoritarian revision—a snare or tangled rope with extraction. The engine computes this divergence: the same structural data (beneficiaries + victims + active enforcement) yields different per-seat classifications. The claimed type (tangled rope) reflects the asymmetry: there is coordination, but it is asymmetric; there are beneficiaries and victims organized by the same rule; active enforcement exists (by legislatures and movements resisting judicial supremacy).
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements and legislative majorities occupy the beneficiary end of directionality (d near 0.0–0.3): they gain interpretive authority, their constitutional readings are elevated to co-equal status with courts, and they face lower exit friction. Constitutional minorities and counter-majoritarian dependents occupy the victim end (d near 0.7–1.0): they lose the security of judicial finality, their rights become dependent on majoritarian political favor, and their exit is blocked (they cannot leave the constitutional order). Judicial finality advocates are also targets in structural terms (d near 0.6–0.8): they lose institutional monopoly, face delegitimization of the judicial supremacy frame, and bear costs of sustained contestation and legislative override. The directionality override for minorities is not needed: the derivation from victim status + trapped exit yields high d naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the need for constitutional meaning to evolve with democratic will) remains live and contested. The constraint does not exhibit mandatrophy—it is not an atrophied remainder of a solved problem. The reading itself is an active contestant in contemporary constitutional law: scholars defend it, movements invoke it, legislatures sometimes enact on it. However, there is a latent mandatrophy risk: if courts successfully entrench judicial supremacy (if the rival originalist or living constitution readings achieve hegemonic status and make popular contestation seem illegitimate), then popular constitutionalism could become a theater piece—formally invoked in movements but unable to shift actual constitutional meaning. That risk is captured in the theater_ratio trajectory: it rises early (0–15) as movements perform popular sovereignty, then plateaus (25–40), suggesting the performative element has stabilized at a fraction of total activity. The constraint is not yet a piton because movements and legislatures still generate real constitutional change (legislative victories, shifts in enforcement practice), but it is vulnerable to becoming one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_threat_to_minorities,
    'Does democratized constitutional interpretation inevitably expose minorities to majoritarian constitutional reinterpretation, or can popular constitutionalism include strong minority protections within its framework?',
    'Historical observation of which movements invoke popular constitutionalism (if only majoritarian coalitions claim it, the threat is real; if minorities and marginalized groups also appeal to popular sovereignty and have successfully protected their interests through mobilization, the framework includes protection mechanisms). Theoretical analysis of whether ''the people'' as a constitutional category can include minority-protective norms.',
    'If majorities-only, the extraction from minorities is structural and unavoidable—the constraint is a snare for minorities. If minorities can also invoke popular constitutionalism and have protection mechanisms, it is a genuinely tangled rope with mixed outcomes per seat. This is the core structural question about the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_threat_to_minorities, empirical, 'Whether popular constitutionalism can protect minorities or necessarily exposes them to majoritarian risk.').

omega_variable(
    judicial_finality_vs_popular_revision,
    'Is the authority conflict between judicial finality and popular revision logically foreclosed (one reading rules out the other in any single framework), or do they coexist as competing legitimate traditions?',
    'If a coherent constitutional framework can be constructed that preserves both judicial final authority over certain domains (rights, federalism limits) and popular authority over others (policy, amendment), then they coexist. If every attempt to blend them produces internal contradiction, they foreclose each other.',
    'If coexistence is possible, all three readings (originalism, living constitution, popular constitutionalism) can coexist as American constitutional traditions. If foreclosed, one reading''s authority claim eliminates the others'' legitimacy—a zero-sum contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_finality_vs_popular_revision, conceptual, 'Whether popular constitutionalism and judicial supremacy are logically compatible or mutually exclusive authority claims.').

omega_variable(
    empirical_status_of_popular_constitutionalism,
    'Is popular constitutionalism a description of how the American Constitution has actually evolved (movements and legislatures have actually changed constitutional meaning), or is it a normative ideal about how it should evolve?',
    'Historical case studies of major constitutional shifts (civil rights, voting rights, labor protections, federalism changes): did courts follow popular mobilization, or did popular mobilization follow courts? If the former, popular constitutionalism describes practice. If the latter, it is prescriptive.',
    'If descriptive, the constraint is embedded in actual American constitutional practice and the rivalry between readings is about intellectual honesty, not fundamental change. If prescriptive, the reading proposes a significant break from settled judicial supremacy and the extraction from minorities is a policy choice, not an inevitable fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_status_of_popular_constitutionalism, empirical, 'Whether popular constitutionalism describes historical practice or prescribes an alternative.').

omega_variable(
    kernel_reading_contest_structure,
    'Which authority grounding—judges'' reasoned judgment (living constitutionalism), framers'' fixed intent (originalism), or democratic mobilization (popular constitutionalism)—is the legitimate source of constitutional meaning?',
    'This is a normative/philosophical question, not empirically resolvable: it depends on one''s theory of constitutional authority and democratic legitimacy. Resolution requires commitment to a framework (judicial expertise vs. popular sovereignty vs. historical fidelity) rather than data.',
    'This omega documents that the three readings reflect fundamentally different commitments about where authority should reside. No empirical finding can resolve it; it is a preference/conceptual question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, preference, 'The irreducible contest among three authority groundings for constitutional meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(us_c_be_t5, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(us_c_be_t15, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(us_c_be_t25, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(us_c_su_t5, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(us_c_su_t15, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(us_c_su_t25, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_interpretive kernel. The sibling readings (originalist_reading, living_constitution_reading) are separate constraint stories with different authority groundings, beneficiary/victim structures, and extractiveness profiles. All three readings share the same kernel (what 'the Constitution' means) but instantiate it differently. Popular constitutionalism influences both siblings by challenging their shared assumption that courts are the final authority; it coexists with both as competing American constitutional traditions. Link all three stories via network.affects_constraints for constraint family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity — Sovereignty Guarantor Reading
 *   domain: political/economic/institutional
 *
 * SUMMARY:
 *   The standing arrangement under contest is the EU Council's unanimity
 *   requirement across sovereignty-implicating files: common foreign and
 *   security policy decisions, taxation, treaty revision, enlargement, own
 *   resources, social-security harmonization, and citizenship. This story
 *   generates ONE reading of that arrangement — the sovereignty-guarantor
 *   reading — under which the requirement is foundational protection against
 *   majoritarian coercion: each government must consent to collective action
 *   that touches its sovereignty, veto use is the legitimate exercise of an
 *   equal right, and the beneficiary set spans all member governments,
 *   weighted toward the smallest. Under this reading's own lights the
 *   arrangement extracts no systematic rents; its costs are coordination
 *   costs (latency, lowest-common-denominator outcomes, negotiation
 *   overhead), which is why epsilon sits at a moderate 0.42 rather than near
 *   zero. The epsilon referent is the unanimity requirement itself as this
 *   reading assesses it — never the qualified-majority alternative this
 *   reading did not choose. Claimed type and metrics are independent authored
 *   facts: the claim is rope because the structure solves a genuine
 *   minority-protection problem with symmetric blocking power and no
 *   suppressed alternatives; the metrics describe what the reading observes.
 *   KEY AGENTS (by structural relationship): - small_member_states: Primary
 *   protected beneficiary (organized/constrained) — hold blocking power equal
 *   to the largest; the regime's core deliverable runs to them -
 *   large_member_states: Secondary beneficiary (institutional/constrained) —
 *   retain absolute veto in vital-interest files at the price of collective
 *   speed - european_commission: Agenda-setter (institutional/constrained) —
 *   tables proposals that must clear every assent; absorbs blockage friction
 *   - integrationist_member_states: Friction-bearing participant
 *   (organized/constrained) — bear delay and dilution while wielding the same
 *   shield - member_state_populations: Diffuse beneficiary with indirect
 *   costs (moderate/mobile) — experience sovereignty protection as national
 *   parliamentary control - candidate_countries: Excluded party
 *   (powerless/trapped) — accession hangs on unanimous existing-member assent
 *   they cannot cast or block - eu_treaty_scholars: Analytical observer
 *   (analytical/analytical) — maps veto episodes and adjudicates between
 *   readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.42).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.18).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity — Sovereignty Guarantor Reading").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "political/economic/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '64e20bfb-f4a0-472f-889b-33e3e1f4dbe5').
narrative_ontology:cs_kernel_codification('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', fixed_text).
narrative_ontology:cs_authority_grounding('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', lineage).
narrative_ontology:cs_interpretation_layer_present('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5').
narrative_ontology:cs_reading_relation('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', eu_council_unanimity__veto_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', foundational, sovereign_consent_precondition).
narrative_ontology:cs_axiom_status(sovereign_consent_precondition, holdable).
narrative_ontology:cs_axiom_grounding('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', sovereign_consent_precondition, deontological).
narrative_ontology:cs_axiom('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', secondary, blocking_is_rights_exercise).
narrative_ontology:cs_axiom_status(blocking_is_rights_exercise, holdable).
narrative_ontology:cs_axiom_grounding('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', blocking_is_rights_exercise, deontological).
narrative_ontology:cs_reference_frame('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', sovereign_equality_consent_rule).
narrative_ontology:cs_drift_state('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', post_lisbon_expansion_of_qmv, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('64e20bfb-f4a0-472f-889b-33e3e1f4dbe5', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, integrationist_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, member_state_populations).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, integrationist_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, member_state_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments of states such as Malta, Cyprus, and the Baltics cast a blocking vote identical to Germany's in the Council formations covering foreign policy, taxation, treaty revision, enlargement, and own resources. Being outvoted on a sovereignty-touching file is structurally unavailable to them; that guarantee is the regime's core deliverable. Leaving means exiting the Union entirely, which carries existential economic and security costs.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    organized, generational, constrained, continental).

% France, Germany, Italy, and Spain retain an absolute veto over the files where their vital interests run deepest — agriculture, defense procurement, nuclear energy, tax bases. They trade slower collective capability for that absolute protection and periodically propose extending majority voting into new domains while insisting on keeping unanimity wherever their own sensitivities lie.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Drafts and tables the legislative proposals that must win every member government's assent in unanimity-bound files. When a single government balks, the Commission absorbs the resulting work: renegotiating packages, lowering ambition, shelving initiatives, and re-tabling across years. It operates entirely inside the procedure it serves; exiting it is not a meaningful option.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, agenda_setter,
    institutional, generational, constrained, continental).

% The Benelux and Nordic governments pressing for deeper common action — joint procurement, migration solidarity, coordinated taxation — watch initiatives stall while awaiting a single outstanding assent. They bear the latency and the diluted ambition of every blocked file, while holding the same blocking shield over their own sensitivities that they wish others would relinquish.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, integrationist_member_states, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, integrationist_member_states, beneficiary).

% Citizens experience the consent regime as their national parliament keeping final say over taxes, defense deployments, and foreign policy positions, and they pay for it as delayed or watered-down joint action: sanction rounds negotiated over months, energy coordination stalled in crises. Free movement gives individuals an exit across borders that their governments, bound by treaty, lack.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, member_state_populations, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, member_state_populations, payer).

% Accession aspirants such as Ukraine, Moldova, and the Western Balkan states need every existing government's assent to join. They hold no vote, cannot block or accelerate anything, and can only meet benchmarks and wait. Their timelines hang on bilateral disputes between existing members that have nothing to do with their readiness.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, candidate_countries, excluded,
    powerless, generational, trapped, continental).

% Legal academics and think-tank analysts map which files still require unanimity, document individual veto episodes and abstention practice, and adjudicate between competing accounts of what the rule protects and whom it empowers. They take no side in the Council and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_treaty_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the Union from binding any member state to collective action touching its sovereignty without that state's consent: foreign policy positions, taxation, treaty revision, accession, and own resources each require every government's agreement. The problem solved is minority protection in an association of sovereign states — no government can be outvoted into obligations it rejects.
% TRANSFER_FUNCTION: Moves decision latency and concession packages from proposing coalitions toward individual hold-out governments: every blocked file converts collective speed into private assurance for whichever state withholds assent. Nothing material is systematically transferred to any standing seat; what moves is time, negotiating attention, and the security of equal blocking power distributed identically to all twenty-seven governments regardless of size.
% ABSENT_VOICES: Candidate countries would object loudest — their futures depend on unanimous existing-member assent they cannot influence. Future generations are locked by treaty-revision rules that themselves require unanimity. Non-governmental and parliamentary voices reach the table only filtered through the twenty-seven executives; the European Parliament debates unanimity's reform but cannot alter it.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished overnight, sovereignty-touching domains would fall to population-weighted voting: small governments would lose their equal blocking power, large-state coalitions would move foreign policy and tax files at will, accession would accelerate or stall at the whim of majorities rather than single governments, and the balance the Union has struck between pooling and autonomy would reorganize around QMV arithmetic within a treaty cycle.
% FOUNDING_PROBLEM: How can sovereign states pool economic and legal functions without surrendering domestic control over the domains where national survival and identity run deepest? The rule descends from the Luxembourg Compromise of 1966, reached when France paralyzed the Community rather than accept outvoting on what it deemed vital interests, and was codified at Maastricht and retained at Lisbon for the most sovereignty-sensitive files.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the small-state beneficiary core: the Intergovernmental Conference records show large states — France above all, in the empty-chair crisis — demanding the consent guarantee despite losing relative voting power under it, and Denmark and the United Kingdom extracting opt-outs during ratification crises. Liberal-intergovernmentalist scholarship (Moravcsik's line of work) documents from outside the institutions that member governments consistently prioritize autonomy in high-stakes domains over collective capability. The persistence of red lines across every enlargement round attests the problem is not settled.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.42: the arrangement's costs are real but unsystematic — blocked files delay collective action and dilute ambition, yet no standing seat captures what is lost; the reading prices these as coordination costs, and the moderate value reflects their cumulative weight across an expanding agenda (sanctions rounds, enlargement steps, own-resources negotiations). Suppression 0.18: participation is voluntary, opt-outs and enhanced cooperation persist, and Article 50 exit exists — the raw structural suppression is low, and it stays low because suppression is an unscaled property of the constraint, not amplified by scope or power. Theater ratio 0.12: the protective function is substantively exercised — vetoes over sanctions, taxation, and accession are serious acts with real consequences, not performance. Accessibility collapse 0.38: alternatives remain workable once the rule is understood — enhanced cooperation, intergovernmental tracks outside the treaty framework, and majority voting where the domain permits — so alternatives are costly but not collapsed. Resistance 0.55: sustained pressure from the Parliament, the Conference on the Future of Europe, and large-state coalitions seeks to migrate remaining files to QMV, meeting organized small-state defense of the rule. The measurement series share one grid (points 0, 6, 12, 18, 24, 30 of an interval running 1993–2023, Maastricht entry into force to the contemporary period) so every tracked metric is authored at every examined time point; base_extractiveness rises as the Union's agenda presses harder against unanimity-bound domains, theater rises mildly as invocation of sovereignty becomes more symbolic, and suppression_requirement declines as constructive abstention, passerelle clauses, and QMV expansion thin the rule's active blocking force.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently from identical structural inputs. From the small_member_states seat the arrangement is near-pure protection: equal blocking power is the deliverable, and the engine derives low directionality and negligible effective extraction. From the integrationist_member_states seat the same rule is a standing latency cost — they bear every blocked file's delay and dilution while receiving the identical shield, so the engine should compute mild cost-bearing without a target signature. The european_commission seat experiences the agenda-setting role inverted: it sets the agenda but cannot close it, absorbing blockage as workload. Candidate countries sit wholly outside the conversation — the rule allocates them no seat at all. Same-power differentiation is visible between the two member-state classes: identical nominal veto rights, differentiated by how often each class needs the collective to move fast.
 *
 * DIRECTIONALITY LOGIC:
 *   Both declared beneficiary groups derive directionality near the subsidized end: unanimity guarantees them consent rights they would lack under population-weighted voting, with small states gaining proportionally more. No victim group is declared under this reading because the reading denies systematic extraction — blocked proposers are experiencing the consent regime operating as designed, not being extracted from; their friction is coordination cost, reflected in epsilon's moderate level rather than in a target-directionality seat. Member_state_populations inherit protection indirectly and pay diffuse indirect costs, landing near symmetric. No directionality_overrides are authored: the derivation from beneficiary declarations plus exit options captures every seat's relationship adequately, including the Commission's mixed agenda-setter-with-costs position, which the commentary documents narratively rather than correcting numerically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pooling functions without surrendering control over sovereignty-touching domains — remains live, so no mandatrophy is declared and the founding_problem_status is live with external corroboration. What would flip this story toward obsolescence: if passerelle clauses and successive treaty revisions emptied the unanimity domains until the guarantee covered nothing consequential, the rule would persist as form without protective function — a vestigial signature with rising theater. Conversely, the classification apparatus guards against the opposite mislabeling: reading this rule as a snare (as the veto-trap sibling reading does) mistakes consent-priced coordination for extraction. The discriminator the engine can verify is symmetry — every government holds the identical blocking right, no seat captures what blocking withholds, and the beneficiary declaration spans both size classes — which distinguishes this rope from a structure in which blocking concentrates gains in a standing holder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_blocking,
    'This constraint is the sovereignty_guarantor_reading of kernel eu_council_unanimity; the sibling readings instantiate different constraints over the same treaty text. Is blocking a rights-exercise costing only coordination (this reading, epsilon 0.42, no victims), a minoritarian extraction instrument (veto_trap_reading, high epsilon, identifiable victims), or a legitimacy-production input (diplomatic_capital_reading, low epsilon)? The disagreement is located precisely in the normative characterization of the blocking act.',
    'Cross-reading corpus comparison: compile all three stories of the kernel and compare per-seat classifications, epsilon values, and victim declarations; adjudicate which characterization matches observed veto-behavior distributions rather than re-measuring the treaty text, which is identical across readings.',
    'Under veto_trap_reading the same rule computes as snare or tangled_rope with collective-action-favoring governments as victims; under diplomatic_capital_reading it computes near-pure rope with negligible epsilon; this story''s rope classification with moderate epsilon holds only within the sovereignty-guarantor seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_blocking, conceptual, 'Committer-frame routing: one kernel, three readings, three constraints with distinct epsilon; this file is the sovereignty-guarantor seat.').

omega_variable(
    holdout_motivation_ambiguity,
    'Are individual blocking episodes exercises of sovereignty defense, as this reading characterizes them, or bargaining moves aimed at extracting concessions disproportionate to any sovereignty interest at stake?',
    'Code veto episodes across the interval (CFSP decisions stalled by single governments, own-resources negotiations, sanctions-package holdouts) for stated justification versus extracted side-payments and package sweeteners; a stable fraction of blocks yielding concessions untethered from stated sovereignty concerns would strain the rights-exercise characterization.',
    'Recurrent rent-seeking blocks would raise effective extraction beyond the coordination-cost band and pull the computed type toward tangled_rope even within this reading''s own seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holdout_motivation_ambiguity, empirical, 'Whether observed veto behavior matches the sovereignty-defense characterization this reading asserts.').

omega_variable(
    constructive_abstention_hollowing,
    'Does constructive abstention — adoption proceeding when abstaining governments decline to block — preserve the consent guarantee this reading celebrates, or hollow it into a default-adoption rule with a residual veto?',
    'Legal-institutional analysis of CFSP adoption practice since the Nice provisions: count decisions adopted over abstentions and examine whether abstaining governments treat abstention as tacit consent or as reserved dissent backed by later non-implementation.',
    'If abstention functions as tacit consent in practice, the protection is thinner than the formal rule suggests and the constraint drifts toward vestigial-form characteristics — the guarantee preserved theatrically while its operative force decays, consistent with the falling suppression_requirement series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructive_abstention_hollowing, conceptual, 'Whether the consent guarantee survives the abstention softening in operative terms.').

omega_variable(
    exit_credibility_post_brexit,
    'How much does Article 50 withdrawal — demonstrated at scale by the United Kingdom''s departure — function as a credible exit option modulating member governments'' exposure to the unanimity regime, versus a nominal right whose realized costs deter any use?',
    'Compare member-government bargaining behavior over unanimity-bound files before and after Brexit, and audit the realized economic and security costs the departing state bore.',
    'Highly realized exit costs mean governments are more constrained than nominal treaty rights suggest, nudging effective extraction upward and weakening the voluntary-association framing behind the low suppression score; demonstrably cheap exit would confirm it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_credibility_post_brexit, empirical, 'Credibility of treaty exit as the alternative that keeps suppression low.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(eu_c_tr_t6, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(eu_c_tr_t18, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eu_c_be_t6, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(eu_c_be_t18, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 18, 0.37).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement(eu_c_su_t6, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 6, 0.24).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(eu_c_su_t18, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 18, 0.21).
narrative_ontology:measurement(eu_c_su_t24, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 24, 0.19).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'EU Council unanimity' per the epsilon-invariance principle: one treaty kernel, three structurally distinct constraints. This file (sovereignty_guarantor_reading) authors blocking as rights-exercise, epsilon 0.42, beneficiaries spanning both member-state size classes, no victims. eu_council_unanimity__veto_trap_reading authors the same rule as a minoritarian extraction instrument — high epsilon, identifiable victims among collective-action-favoring states, likely snare or tangled_rope classification. eu_council_unanimity__diplomatic_capital_reading authors it as a legitimacy-producing negotiation requirement — low epsilon, consensus quality as the delivered good. The upstream text-level fact (which domains formally require unanimity) conditions all three downstream readings; each downstream file links back through affects_constraints so contamination and cross-reading comparison can traverse the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

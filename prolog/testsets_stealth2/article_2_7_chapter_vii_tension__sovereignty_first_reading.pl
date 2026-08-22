% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Sovereignty-First Reading of the Charter Intervention Gate
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   Within the article_2_7_chapter_vii_tension kernel, this story
 *   instantiates the sovereignty_first_reading: state sovereignty is the
 *   foundational constitutive rule of international order, and coercive
 *   international intervention in domestic affairs is lawful only with the
 *   target state's explicit consent or upon Security Council authorization
 *   under Chapter VII, which this reading confines to inter-state aggression
 *   and threats to international peace narrowly construed. The standing
 *   arrangement under contest is the Charter's non-intervention default with
 *   its permanent-member-gated exception machinery. Assessed in its own terms
 *   the reading presents the arrangement as the load-bearing wall of the
 *   post-1945 peace; assessed structurally, the same wall shields atrocity
 *   perpetrators and taxes the populations they prey on. This file is one
 *   epsilon-invariant reading: the sibling r2p_reading (conditional
 *   sovereignty, atrocity-triggered responsibility) is a separate constraint
 *   with its own victim set and its own epsilon, linked through
 *   network.affects_constraints. The disagreement between readings is located
 *   in one structural element: whether systematic domestic atrocity falls
 *   inside threat-to-international-peace, and therefore inside the gate this
 *   reading administers. KEY AGENTS (by structural relationship): -
 *   permanent_five_members: agenda-setter and gatekeeper
 *   (institutional/arbitrage) — controls which crises count as international;
 *   collects gatekeeping rents - post_colonial_states: primary beneficiary
 *   (organized/constrained) — holds the anti-predation shield -
 *   authoritarian_regimes: beneficiary (organized/constrained) — converts the
 *   shield into impunity - populations_under_domestic_atrocity: primary
 *   target (powerless/trapped) — bears the arrangement's full cost with no
 *   standing - humanitarian_ngos: excluded advocate (organized/constrained) —
 *   supplies the evidence, holds no vote - security_council_elected_members:
 *   junior participant (institutional/constrained) — votes without veto,
 *   absorbs blame - international_court_of_justice: analytical observer
 *   (institutional/analytical) — affirms the norm in principle, cannot reach
 *   the powerful
 *
 * KEY AGENTS:
 *   - permanent_five_members: agenda-setter and gatekeeper (institutional/arbitrage) — controls which crises count as international; collects gatekeeping rents
 *   - post_colonial_states: primary beneficiary (organized/constrained) — holds the anti-predation shield
 *   - authoritarian_regimes: beneficiary (organized/constrained) — converts the shield into impunity
 *   - populations_under_domestic_atrocity: primary target (powerless/trapped) — bears the arrangement's full cost with no standing
 *   - humanitarian_ngos: excluded advocate (organized/constrained) — supplies the evidence, holds no vote
 *   - security_council_elected_members: junior participant (institutional/constrained) — votes without veto, absorbs blame
 *   - international_court_of_justice: analytical observer (institutional/analytical) — affirms the norm in principle, cannot reach the powerful
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.7).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Sovereignty-First Reading of the Charter Intervention Gate").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '93ab8ee4-408f-4222-96bc-f59925785ca0').
narrative_ontology:cs_kernel_codification('93ab8ee4-408f-4222-96bc-f59925785ca0', fixed_text).
narrative_ontology:cs_authority_grounding('93ab8ee4-408f-4222-96bc-f59925785ca0', extraction).
narrative_ontology:cs_interpretation_layer_present('93ab8ee4-408f-4222-96bc-f59925785ca0').
narrative_ontology:cs_reading_relation('93ab8ee4-408f-4222-96bc-f59925785ca0', article_2_7_chapter_vii_tension__r2p_reading, forecloses).
narrative_ontology:cs_axiom('93ab8ee4-408f-4222-96bc-f59925785ca0', foundational, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('93ab8ee4-408f-4222-96bc-f59925785ca0', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_axiom('93ab8ee4-408f-4222-96bc-f59925785ca0', foundational, force_requires_collective_authorization).
narrative_ontology:cs_axiom_status(force_requires_collective_authorization, holdable).
narrative_ontology:cs_axiom_grounding('93ab8ee4-408f-4222-96bc-f59925785ca0', force_requires_collective_authorization, conventional).
narrative_ontology:cs_axiom('93ab8ee4-408f-4222-96bc-f59925785ca0', secondary, atrocity_not_per_se_international_threat).
narrative_ontology:cs_axiom_status(atrocity_not_per_se_international_threat, holdable).
narrative_ontology:cs_axiom_grounding('93ab8ee4-408f-4222-96bc-f59925785ca0', atrocity_not_per_se_international_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('93ab8ee4-408f-4222-96bc-f59925785ca0', westphalian_charter_settlement).
narrative_ontology:cs_drift_state('93ab8ee4-408f-4222-96bc-f59925785ca0', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('93ab8ee4-408f-4222-96bc-f59925785ca0', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_elected_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_elected_members).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, non_intervention_norm).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, domestic_jurisdiction_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over every Chapter VII decision and thereby determine which crises get classified as threats to international peace. Are structurally immune from the constraint's application against themselves, since any enforcement action targeting a permanent member requires that member's own vote. When the gate blocks action they oppose they cite the Charter; when they want to act without it they build coalitions outside UN auspices (Kosovo 1999, Iraq 2003) and absorb the legal criticism. Collect gatekeeping rents: alliance partners and aid recipients bid for their protection and their veto.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members, beneficiary).

% Won independence inside this legal order and regard the non-intervention shield as their principal guarantee against great-power predation. Caucus defensively (G77, Non-Aligned Movement) against any widening of the intervention gateway, including humanitarian exceptions. Bear indirect costs when neighboring atrocities generate refugee flows and regional instability that the frozen gate leaves unaddressed, but judge the predation risk of loosening the rule higher than the atrocity risk of keeping it shut.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    organized, generational, constrained, global).

% Depend on the exclusivity of domestic jurisdiction for regime survival: the same rule that blocks humanitarian intervention blocks external accountability for internal repression. Mobilize bloc votes in UN bodies to keep atrocity situations framed as internal matters, and cultivate permanent-member patrons whose vetoes extend the shield. Exiting the arrangement would mean submitting to the external scrutiny they exist to avoid.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    organized, biographical, constrained, global).

% Face mass killing, expulsion, or starvation carried out by their own state or amid its collapse. Under this reading they hold no standing anywhere in the system: no petition right, no protected channel, no appeal. Relief arrives only if their own government consents or if a Security Council majority without a hostile veto classifies their suffering as an international threat. Flight means closed borders and camps; staying means the atrocity. The arrangement's protection reaches them only through their attacker's consent.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Document atrocities, warn of imminent mass violence, and campaign for protective action. They testify before commissions of inquiry and brief delegations, but hold no vote, no veto, and no standing in the authorization process their evidence is meant to inform. Their advocacy is admitted as background material and excluded from decision authority.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_ngos, excluded,
    organized, biographical, constrained, global).

% Serve two-year rotating seats with full vote but no veto. Frequently abstain or issue explanations of vote against permanent-member-drafted positions, yet cannot block or pass anything alone. Bear the reputational cost of collective inaction while collecting the procedural benefit of a seat at the only lawful-force gate.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_elected_members, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_elected_members, beneficiary).

% Adjudicates boundary disputes about what counts as intervention and has affirmed the non-intervention norm in principle (Nicaragua v. United States), but holds no compulsory jurisdiction over the powerful, cannot review Security Council action, and its rulings bind only the parties that accept them. Its jurisprudence absorbs interpretive drift without ever resolving the underlying contest over the kernel.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative gate for the lawful use of interstate force: one forum, one vote threshold, predictable criteria, and a standing prohibition that lets roughly 190 states plan their security without arming against every neighbor's future rescue. Solves the collective-action problem of restraining great-power military intervention under universalist pretexts while keeping the great powers inside a common legal frame.
% TRANSFER_FUNCTION: Moves protection away from populations under domestic atrocity, who receive none unless their own attacker consents or a veto-free Council majority acts, and converts it into immunity for governing elites and gatekeeping privilege for the permanent five. Moves the decision cost of every humanitarian crisis onto the victims who wait, and the reputational cost of inaction onto junior Council members.
% ABSENT_VOICES: Populations under atrocity are the structurally absent voice: no petition right, no standing before the Council, no vote in any organ, no representation except through states that may be their attackers. Humanitarian agencies and affected diasporas speak in corridors and press conferences but hold no authorization power; their objection, protection now, is inadmissible under the reading's own terms, which is precisely what the exclusion machinery maintains.
% DISAPPEARANCE_RATIONALE: If the gate vanished overnight, the architecture of lawful force collapses with it: hundreds of deployments lose their legal basis, alliance guarantees keyed to Council authority reopen, and either unilateral humanitarian wars proliferate or an emergency replacement authorization mechanism is rebuilt within months. The permanent five's gatekeeping rents, the sovereignty caucus's shield, and the victims' abandonment would all be rearranged simultaneously; nothing about the current arrangement survives its own removal.
% FOUNDING_PROBLEM: The founders' problem at San Francisco: outlaw aggressive war permanently while preventing the enforcement machine from becoming a great-power license for domination. Restrain war without licensing predation, and protect the weak state's internal existence from the strong state's universalist excuses.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties by the San Francisco drafting record as documented by neutral legal historians, in which small-state delegates explicitly demanded insulation from great-power domination, and by post-colonial international-law scholarship independent of any current beneficiary. No source free of the benefiting parties attests that the problem remains live in its original inter-state form: conflict-data series (UCDP/PRIO) show intra-state atrocity now dominates the harm landscape the arrangement governs, and that shift is documented by parties with no stake in either reading.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.70) because the arrangement's operative effect is to convert atrocity-zone protection into elite immunity: the good it withholds, relief from mass violence, is withheld precisely from those with no substitute channel, while the coordination it sells, non-predation, is priced onto third parties. Suppression (0.78) is authored as a raw structural property, unscaled by power or scope in the engine's computation, because persistence depends on actively manned machinery: veto discipline, sovereignty-caucus mobilization in UN bodies, and the legal impossibility of lawful unilateral rescue. Theater (0.44) reflects a large performative layer — annual never-again rhetoric, R2P summit language adopted in 2005 and operationally gutted, commissions of inquiry whose findings feed no action path — wrapped around a functional gatekeeping core. Accessibility collapse (0.58): alternatives do not vanish for states, since coalitions have acted outside UN auspices and consent-based operations continue, but for the victim seat every alternative collapses to zero. Resistance (0.62) is real and organized: the R2P movement, veto-restraint codes, and humanitarian-access litigation all contest the reading's terms. The measurement series run on one shared nine-point grid with all three metrics authored at every point, so no end-state value is silently substituted into earlier decades. Trajectories are monotonic rather than cyclical: extraction climbs with decolonization's expansion of the shield-beneficiary class and with the post-Cold War migration of mass violence from inter-state to intra-state forms; theater peaks around the 2005 World Summit compromise and eases slightly as veto-restraint initiatives add partial substance; suppression rises as the gate, no longer sheltered by Cold War paralysis, requires harder active defense through the Syria-era vetoes and bloc discipline. Atrocity populations cannot form the coalition that powerless-agent analysis normally checks for: they are dispersed, targeted, and stripped of standing by design, so no coalition correction applies.
 *
 * PERSPECTIVAL GAP:
 *   One text, three experientially different constraints. From the permanent-five seat the arrangement is prerogative: a discretionary gate they operate and are exempt from. From the post-colonial seat it is the shield that stands between them and the previous century's gunboats — genuine, hard-won coordination. From the atrocity-population seat it is abandonment administered by procedure: the same rule that protects the besieger strips the besieged of recourse. The engine computes these divergent per-seat classifications from the structural data; the divergence is the finding, not noise. The payer seat's experience is invisible in the arrangement's own documents, because no organ records an objection from a party that holds no seat from which to lodge one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Authoritarian regimes sit nearest the pure-beneficiary pole: the arrangement subsidizes them exclusively through impunity, and their constrained exit means only that exiting equals submitting to scrutiny. Permanent five members derive low directionality as declared beneficiaries, with arbitrage-grade exit pushing them further toward the beneficiary end, since they can route around the gate at will and are structurally immune to its application against themselves. Post-colonial states derive low-but-not-zero directionality: they collect the shield while carrying spillover costs, refugee flows and regional contagion, that the frozen gate leaves on their territory. Populations under domestic atrocity derive near-full-target directionality: trapped, powerless, and without standing, they bear the entire extracted good. Humanitarian NGOs sit target-side without being victims: they expend resources the arrangement neutralizes. Elected Council members sit mildly target-side, their agency taxed and blame absorbed; the Court sits at the analytical pole.
 *
 * MANDATROPHY ANALYSIS:
 *   Two misclassification risks bracket this story. Calling it a snare would erase the genuine coordination function: the anti-predation shield is real, valued by dozens of states with no atrocity to hide, and historically grounded in actual gunboat practice; a snare verdict would license tearing down the one rule weak states trust. Accepting the reading's own mountain-flavored framing, sovereignty as bedrock and the arrangement as natural order, would immunize the extraction from scrutiny entirely, since naturality claims are exactly how beneficiaries launder constructed arrangements. Tangled rope holds both truths: coordination and extraction run through the same clause, enforcement is real, and the victim set is identifiable. On the genealogy: the founding problem, restraining inter-state aggressive war, has partially drifted out from under the arrangement, because the dominant mass violence of the past three decades is intra-state, the precise category the gate was built not to touch. Hence founding_problem_status is contested rather than dead: the old problem lingers, but the arrangement's operative center of gravity no longer matches the harm landscape it governs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the sovereignty_first_reading of the article_2_7_chapter_vii_tension kernel; what structurally changes if the r2p_reading governed instead?',
    'Classify the sibling story (article_2_7_chapter_vii_tension__r2p_reading) on its own structural data and compare victim sets, beneficiary sets, and epsilon; the delta is the committer structure made measurable.',
    'Under the sibling reading the victim set contracts, since populations gain a protection claim, and the beneficiary set shifts, since shield-states lose unconditional immunity; this reading''s high extraction is indexical to sovereignty-first, not a property of the Charter text itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which reading of the kernel this constraint instantiates and what the sibling would change.').

omega_variable(
    sovereignty_naturalness_ambiguity,
    'Is state sovereignty a genuine structural feature of political reality, mountain-like bedrock as the reading''s rhetoric treats it, or a constructed, revisable legal arrangement maintained by identifiable beneficiaries?',
    'Test revisability against the Charter''s own amendment machinery (Articles 108-109, permanent-member consent required) and against historical mutation of the sovereignty norm through humanitarian-access doctrine and individual criminal liability; a rule alterable by its own beneficiaries'' procedures is constructed.',
    'If constructed, the arrangement classifies as enforced coordination-plus-extraction and loses any natural-law immunity; if treated as bedrock, its extraction becomes invisible behind naturality claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_naturalness_ambiguity, conceptual, 'Natural-law versus constructed status of the sovereignty foundation.').

omega_variable(
    atrocity_spillover_empirics,
    'Does systematic domestic atrocity in fact constitute a threat to international peace — do the effects reliably cross borders?',
    'Conflict-spillover analysis using refugee-flow data, regional conflict-diffusion studies, and UCDP/PRIO series to test whether atrocity episodes predict cross-border instability at rates comparable to inter-state war.',
    'If spillover is systematic, the reading''s own limiting condition erodes from within: the Chapter VII gate opens for atrocity cases on the reading''s own text, and enforcing the exclusion becomes interpretation rather than law. If contained, the narrow gate is textually faithful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_spillover_empirics, empirical, 'Whether the gate''s limiting condition matches the empirical geography of atrocity harm.').

omega_variable(
    predation_counterfactual_baseline,
    'How much predation does the non-intervention default actually prevent — what is the counterfactual rate of humanitarian-pretext wars absent the rule?',
    'Historical counterfactual analysis of pre-1945 intervention practice and of episodes where the gate was bypassed (Kosovo 1999, Iraq 2003): frequency, stated pretexts, and outcomes relative to gate-respecting periods.',
    'If pretext wars were rare or contained absent the rule, the coordination justification thins and effective extraction rises; if they were endemic, part of the measured extraction is the genuine price of protection and the tangled-rope reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(predation_counterfactual_baseline, empirical, 'Size of the real coordination benefit against which extraction must be netted.').

omega_variable(
    p5_gatekeeping_necessity,
    'Is permanent-five gatekeeping a coordination necessity, great-power buy-in as the price of any functioning collective-security machine, or concentrated rent collection?',
    'Compare enforcement throughput and compliance under the veto design against alternative designs'' historical performance (League of Nations unanimity, coalition-of-the-willing practice); assess whether veto removal collapses participation or merely redistributes it.',
    'If necessity, part of permanent-member privilege is coordination cost belonging below the extraction line; if rent, the gatekeeping layer is extractive overhead stacked on the coordination function and the capture verdict sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_gatekeeping_necessity, conceptual, 'Whether the veto layer is the price of the machine or a tax on it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.14).
narrative_ontology:measurement_basis(arti_tr_t1945, observed).
narrative_ontology:measurement(arti_tr_t1955, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1955, 0.17).
narrative_ontology:measurement_basis(arti_tr_t1955, observed).
narrative_ontology:measurement(arti_tr_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1965, 0.21).
narrative_ontology:measurement_basis(arti_tr_t1965, observed).
narrative_ontology:measurement(arti_tr_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement_basis(arti_tr_t1975, observed).
narrative_ontology:measurement(arti_tr_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1985, 0.29).
narrative_ontology:measurement_basis(arti_tr_t1985, observed).
narrative_ontology:measurement(arti_tr_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1995, 0.36).
narrative_ontology:measurement_basis(arti_tr_t1995, observed).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.43).
narrative_ontology:measurement_basis(arti_tr_t2005, observed).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(arti_tr_t2015, observed).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2025, 0.44).
narrative_ontology:measurement_basis(arti_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement_basis(arti_be_t1945, observed).
narrative_ontology:measurement(arti_be_t1955, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1955, 0.48).
narrative_ontology:measurement_basis(arti_be_t1955, observed).
narrative_ontology:measurement(arti_be_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1965, 0.53).
narrative_ontology:measurement_basis(arti_be_t1965, observed).
narrative_ontology:measurement(arti_be_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1975, 0.56).
narrative_ontology:measurement_basis(arti_be_t1975, observed).
narrative_ontology:measurement(arti_be_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1985, 0.59).
narrative_ontology:measurement_basis(arti_be_t1985, observed).
narrative_ontology:measurement(arti_be_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement_basis(arti_be_t1995, observed).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement_basis(arti_be_t2005, observed).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement_basis(arti_be_t2015, observed).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2025, 0.7).
narrative_ontology:measurement_basis(arti_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement_basis(arti_su_t1945, observed).
narrative_ontology:measurement(arti_su_t1955, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1955, 0.53).
narrative_ontology:measurement_basis(arti_su_t1955, observed).
narrative_ontology:measurement(arti_su_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1965, 0.57).
narrative_ontology:measurement_basis(arti_su_t1965, observed).
narrative_ontology:measurement(arti_su_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement_basis(arti_su_t1975, observed).
narrative_ontology:measurement(arti_su_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement_basis(arti_su_t1985, observed).
narrative_ontology:measurement(arti_su_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1995, 0.69).
narrative_ontology:measurement_basis(arti_su_t1995, observed).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement_basis(arti_su_t2005, observed).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement_basis(arti_su_t2015, observed).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement_basis(arti_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label of the Article 2(7)/Chapter VII tension conflates two structurally distinct readings of one kernel. This file authors the sovereignty_first_reading: unconditional sovereignty, exclusive domestic jurisdiction, gate confined to inter-state aggression, high extraction borne by atrocity populations and collected by shielded regimes and gatekeeping powers. The sibling article_2_7_chapter_vii_tension__r2p_reading authors conditional sovereignty with an atrocity-triggered responsibility: different victim set, different epsilon. Neither reading is cleanly upstream of the other: each cites the Charter text as evidence for itself and defines itself against the other. They are linked so contamination and drift propagate across the family rather than hiding inside one averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

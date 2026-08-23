% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Prohibition as Revocable Positive Law (Sovereign Override Reading)
 *   domain: constitutional/political_history
 *
 * SUMMARY:
 *   The standing arrangement under contest is the Salic prohibition itself:
 *   the rule that succession to the dynastic monarchies of western and
 *   central Europe passes exclusively through male lines, excluding both
 *   women and descendants through women. Anchor instantiations in the modeled
 *   interval: the French doctrine of fundamental laws and the Habsburg
 *   Pragmatic Sanctions of 1713/1723, by which Charles VI enacted a single
 *   indivisible order of succession admitting his daughters if the male line
 *   failed. THIS story instantiates the sovereign_override_reading only: the
 *   prohibition is valid, binding, duly enacted positive law — and therefore
 *   revocable or replaceable by a competent sovereign-legislative act; a
 *   settlement once sanctioned and sworn is legitimate title, and those who
 *   attack it by arms (Bavaria in 1741, Brandenburg-Prussia seizing Silesia
 *   in 1740 under a marginal title) are, in this reading's frame, rebels
 *   against legitimate authority rather than holders of a rival right. Per
 *   the kernel-reading rule, the eps referent is the standing prohibition
 *   arrangement as this reading assesses it — neither the immutable reading's
 *   natural law nor the cognatic reading's endorsed reversion. Claim and
 *   metrics are independent authored facts: the claimed type is what the
 *   authoring seat believes structurally true; the metrics describe how the
 *   arrangement actually operated across 1713-1748 (t0 = first Pragmatic
 *   Sanction draft, t35 = Treaty of Aix-la-Chapelle, five-year grid).
 *
 * KEY AGENTS:
 *   - - sovereign_legislative_authority: Agenda-setter (institutional/arbitrage) — enacts and revises the succession settlement; owns its defense
 *   - - reigning_agnatic_house: Primary beneficiary (powerful/identity_locked) — inherits ahead of more senior kin descended through women
 *   - - female_line_claimants: Primary target (organized/trapped) — genealogically senior claims voided regardless of proximity of blood
 *   - - dynastic_women: Structural target (moderate/identity_locked) — categorically barred from succeeding or transmitting succession rights
 *   - - rival_claimant_powers: Resisting target (organized/trapped) — converts voided claims into wars (Silesia 1740)
 *   - - crownland_estates: Coordinated participant (organized/constrained) — ratifies settlements, supplies the men and money that defend them
 *   - - agnatic_cadet_branches: Secondary beneficiary (powerful/identity_locked) — appanages, commands, and reversion hopes under the same order
 *   - - dynastic_law_jurists: Analytical observer (analytical/analytical) — parlementary lawyers and publicists who see the whole structure and arm every other seat with doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.58).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Prohibition as Revocable Positive Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, 'a76b96a2-31a2-4cd7-838e-ffbc31c96659').
narrative_ontology:cs_kernel_codification('a76b96a2-31a2-4cd7-838e-ffbc31c96659', formalized).
narrative_ontology:cs_authority_grounding('a76b96a2-31a2-4cd7-838e-ffbc31c96659', practice).
narrative_ontology:cs_interpretation_layer_present('a76b96a2-31a2-4cd7-838e-ffbc31c96659').
narrative_ontology:cs_reading_relation('a76b96a2-31a2-4cd7-838e-ffbc31c96659', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('a76b96a2-31a2-4cd7-838e-ffbc31c96659', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('a76b96a2-31a2-4cd7-838e-ffbc31c96659', foundational, succession_rules_are_revocable_positive_statute).
narrative_ontology:cs_axiom_status(succession_rules_are_revocable_positive_statute, holdable).
narrative_ontology:cs_axiom_grounding('a76b96a2-31a2-4cd7-838e-ffbc31c96659', succession_rules_are_revocable_positive_statute, conventional).
narrative_ontology:cs_axiom('a76b96a2-31a2-4cd7-838e-ffbc31c96659', secondary, sanctioned_settlement_confers_legitimate_title).
narrative_ontology:cs_axiom_status(sanctioned_settlement_confers_legitimate_title, holdable).
narrative_ontology:cs_axiom_grounding('a76b96a2-31a2-4cd7-838e-ffbc31c96659', sanctioned_settlement_confers_legitimate_title, conventional).
narrative_ontology:cs_reference_frame('a76b96a2-31a2-4cd7-838e-ffbc31c96659', enacted_positive_succession_order).
narrative_ontology:cs_drift_state('a76b96a2-31a2-4cd7-838e-ffbc31c96659', aix_la_chapelle_settlement, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a76b96a2-31a2-4cd7-838e-ffbc31c96659', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_agnatic_house).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, agnatic_cadet_branches).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, crownland_estates).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_line_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, dynastic_women).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, rival_claimant_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, crownland_estates).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, sovereign_legislative_supremacy_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, agnatic_priority_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The crown in its legislative capacity. Issues ordonnances and pragmatic sanctions that fix or revise the order of succession; has them registered by courts, ratified by the estates, and guaranteed by foreign powers. Gains the ability to determine who reigns next and to reshape the rule itself — the one seat that can lawfully change the arrangement rather than merely operate under it. Carries the burden of defending whatever settlement it enacts, by diplomacy first and by arms when challenged, and is bound by the expectation, once a settlement is published and sworn, that it will stand behind it.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, sovereign_legislative_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% The male-line dynasty holding the thrones. Each exclusion of a female line places its members ahead of more senior kin descended through women, and concentrates the whole inheritance in one branch instead of partitioning it. Its rank, marriage alliances, and self-understanding are built on the agnatic order; stepping outside it would mean surrendering the standing the order confers. When the settlement is attacked it supplies the armies, the treasury, and the legitimacy claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_agnatic_house, beneficiary,
    powerful, generational, identity_locked, continental).

% Junior male lines — princes of the blood, archducal cadets. They hold appanages, military commands, and contingent hopes of reversion under the same order that favors the senior line. Their identity and prospects exist only inside the dynastic framework; they defend it in council and sometimes in print, but they do not set it.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, agnatic_cadet_branches, beneficiary,
    powerful, generational, identity_locked, national).

% Foreign princely houses descended through royal daughters — the Bavarian and Savoyard lines through the Spanish princesses, the English line through Isabella of France in the previous century. They hold genealogically strong claims that the prohibition voids regardless of proximity of blood. Their realistic choices are to litigate a title the courts will not hear, to renounce for compensation — a renunciation whose own binding force is permanently in dispute — or to press the claim by war and bear its costs.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_line_claimants, payer,
    organized, generational, trapped, continental).

% Princesses and archduchesses of the reigning houses. Raised inside the court order, married as instruments of alliance policy, and categorically unable to succeed or to transmit a right their brothers hold. Some wield considerable informal power — regencies, correspondence networks, brokerage between courts — but none can bring a succession claim, and their acquiescence to their own exclusion is assumed rather than asked.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, dynastic_women, payer,
    moderate, biographical, identity_locked, national).

% Diets and estates of the composite realms — the Hungarian counties, the Austrian and Bohemian lands, the provincial estates. They ratify succession settlements in exchange for confirmed liberties and a guarantee against partition, then supply the taxes and soldiers when the settlement is tested. Refusal is available but expensive; acceptance purchases protection of the realm's integrity at the price of funding its defense.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, crownland_estates, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, crownland_estates, payer).

% Ambitious neighboring powers invoking a dynastic connection against the settlement — Brandenburg-Prussia occupying Silesia in 1740 under a marginal title, Electoral Bavaria claiming the whole inheritance. They convert a voided claim into a casus belli, pay for the war, and keep whatever conquest yields; their challenges are answered as rebellion against legitimate authority rather than heard as rival titles, and once committed to arms they cannot withdraw without returning what they took.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_claimant_powers, payer,
    organized, biographical, trapped, continental).

% Parlementary lawyers, chancery publicists, and university jurists arguing what the succession rules are — whether fundamental laws bind the prince, whether renunciations bind, what the Frankish precedent means for crowns. They command no troops and decide nothing directly, but their doctrines are the ammunition every other seat fires; from this seat the entire structure — enactment, ratification, nullification, enforcement — is visible at once.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, dynastic_law_jurists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, reigning_agnatic_house).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single pre-committed order of succession across a multi-kingdom composite monarchy before any vacancy occurs, converting each potential succession dispute into a settled legal question and preventing partition of the patrimony among plural heirs.
% TRANSFER_FUNCTION: Moves sovereign office and the indivisible dynastic patrimony from senior cognatic claimants and from women as a class to the nearest agnatic male line; secondarily moves the costs of legitimacy-defense — taxation, conscription, and war — onto the ruled populations of the signatory realms.
% ABSENT_VOICES: Dynastic women themselves sat in no diet or parlement that adjudicated the rules excluding them; their consent was presumed, never solicited, and they would object if seated. The rural and urban populations who fought and funded the succession wars likewise had no seat at any table where the settlement was made. Both groups were structurally absent rather than silenced mid-argument.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, every composite monarchy would face an immediate partition crisis: multiple plausible heirs by old blood-proximity rules, revived cognatic claims from foreign houses, and no pre-committed answer. The 1740 vacancy shows the signature of the rearrangement even WITH a settlement in place — four powers went to war; without one, the Habsburg lands revert toward their older partition customs and the French crown toward open contest among the principal claimants.
% FOUNDING_PROBLEM: Prevent partition of the royal domain and recurrent succession warfare among heirs — including sons of daughters — by fixing one unambiguous order of succession before vacancies occur. The problem was posed for France by the crises of 1316-1328 and for the Habsburg monarchy by the scatter of crowns that custom would otherwise divide among many children.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the ruling houses: estate ratification records show the Hungarian and Austrian estates accepting the pragmatic sanction precisely because they feared partition and war, trading acceptance for confirmed liberties; the treaty texts of the period (the Utrecht renunciations, the Aix-la-Chapelle guarantees) show rival and neutral chancelleries alike treating an agreed succession order as a shared necessity worth papering. What no outside party attests is the stronger claim that the agnatic FORM specifically — rather than some fixed order — is necessary; that identification is the dynasty's own.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement transfers sovereign office and indivisible patrimony from genealogically senior cognatic claimants and from women as a class to the nearest agnatic males — a real, recurring transfer borne by identifiable people, but rendered to purchasers of a genuine service (a pre-committed succession order across scattered crowns). Suppression 0.62: persistence rests on legal nullification of claims, the rebel-framing of challengers, and the demonstrated willingness to answer challenge with general war; the mechanism is predominantly structural (juridical voiding plus armed defense) with a smaller internalized component (court-raised women schooled into treating exclusion as the natural order — roughly 70/30 structural to internalized, which the omegas carry rather than the scalar). Theater_ratio 0.22: the core function is real — settlements actually governed who reigned — but a growing share of activity by interval end is genealogical-diplomatic justification work defending the settlement's prestige rather than administering it. Accessibility_collapse 0.48: while the rule stands, the alternative orders (old partition customs, elective schemes, testamentary designation) are foreclosed in practice, but because the reading itself holds the rule to be ordinary statute, a lawful amendment path remains open in principle — alternatives collapse conditionally on sovereign will, not absolutely. Resistance 0.61: sustained across the whole interval and the preceding century — the Hundred Years' War began in exactly this dispute, the Polish Succession war intersected it, and 1740-1748 saw four powers contest the settlement by arms. All three tracked series run on one shared five-year grid with terminal values matching the scalar properties. No cyclical oscillation is modeled: the interval shows monotonic ratchet to the war peak, then partial settlement relief.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the sovereign seat the arrangement is its own instrument — flexible, revisable, the very proof of legislative supremacy. From the female_line_claimant seat the same instrument operates as perpetual confiscation: seniority of blood counts for nothing, renunciation buys nothing reliable, and litigation is hopeless. The estate seat experiences it as an insurance policy bought at ratification price — indivisibility guaranteed, liberties confirmed, taxes levied when the policy pays out. The dynastic women's seat is the starkest divergence: the people most affected held no seat in any diet or parlement adjudicating the rule, and possessed no coalition surface (scattered across courts by marriage, individually embedded in households that enforced conformity), which is why categorical exclusion met no organized resistance despite touching an entire class. The jurist seat alone sees both faces at once — which is precisely why the doctrinal literature oscillates between fundamental-law immutability and legislative-positivist revocability. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (reigning_agnatic_house, agnatic_cadet_branches, crownland_estates) derive low d for those seats; victim declarations (female_line_claimants, dynastic_women, rival_claimant_powers) derive high d, amplified for the trapped exits — a voided claimant has no reliable renunciation channel (see renunciation_irrevocability omega), and identity-lock deepens d for the dynastic women and cadet seats, whose standing exists only inside the order. Continental scope modestly amplifies effective extraction for the targets: a rule spanning many realms is harder to verify and harder to exit than a single-court rule. One override is authored: the institutional power atom to d=0.28. The automatic derivation would place the sovereign_legislative_authority near the beneficiary end (agenda-setter, collects dynastic advantage and legitimation), but the structural relationship is materially less favorable than that: the sovereign bears the arrangement's defense costs directly (four powers went to war over the settlement it enacted), is disciplined by its own doctrine (once legitimacy is staked on enacted-and-sworn settlement, unilateral revision destroys the doctrine that legitimates it), and is locked by estate ratifications and foreign guarantees. The true relationship sits well short of full beneficiary — hence 0.28 rather than a derived value near 0.1.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing partition of composite dynastic patrimonies and recurrent succession warfare by fixing one order before vacancies occur — was live throughout the interval and remains so at t35: the 1740 vacancy demonstrated that even a fully sanctioned, internationally guaranteed settlement still drew four powers to arms, so the problem the arrangement was built for has not died. Mandatrophy is accordingly NOT resolved; no sunset clause exists or should. The classification discipline matters most against two symmetrical misreadings. The immutable reading launders the arrangement as natural or divine order — a false-summit move that would erase the identifiable payers (women, cognatic lines) and immunize the extraction from review. The cognatic reading flattens the arrangement into pure alien imposition — a move that would erase the genuine coordination service (France's succession record after 1328 was markedly less fratricidal than contemporary elective/partition systems) and misread every actor who accepted the settlement as merely deceived. The tangled-rope classification preserves both faces because both are structurally present. On the R5 mismatch check: founding_problem_status = live paired with disappearance_verdict = world_rearranges is the aligned configuration — no zombie flag arises, and none should.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the sovereign_override_reading of the salic_prohibition kernel. What changes structurally if a sibling reading is the better account of the same referent?',
    'Comparative classification across the three linked reading-stories: if immutable_mandate_reading prevails, epsilon collapses toward the natural-law floor, the victim set dissolves (no one ''pays'' a divine mandate), and the family trends toward mountain immunity; if cognatic_reversion_reading prevails, epsilon rises sharply, the coordination function evaporates, and the family trends toward pure imposition.',
    'Determines whether the measured costs on excluded claimants are read as the price of a lawful coordination settlement (this reading), as obedience owed to natural order (immutable reading), or as confiscation under an invalid import (cognatic reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the salic_prohibition kernel correctly characterizes the standing arrangement.').

omega_variable(
    kernel_text_vs_doctrine_scope,
    'Is the kernel the narrow Frankish text (Lex Salica, title 59 on private allodial inheritance, which never mentioned crowns) or the constitutional doctrine of agnatic exclusion constructed on top of it by fourteenth-century French jurists and later statutory enactments?',
    'Textual-genetic analysis: trace what the Pactus Legis Salica actually regulated versus what the 1316-1328 deployments and the later pragmatic sanctions actually governed, and which instrument each reading''s argument depends on.',
    'If the narrow text is the kernel, cognatic_reversion gains decisive force (the text never bound crowns at all) and this reading''s statutory anchor weakens; if the doctrine is the kernel, sovereign_override is its natural home and the text-history objection is beside the point.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_text_vs_doctrine_scope, conceptual, 'Framing under-determination in what counts as the stabilized kernel of the Salic prohibition.').

omega_variable(
    revocability_vs_fundamental_law,
    'Is sovereign legislative competence over the succession rule real in all realms, or regime-contingent — given that the French parlements annulled Louis XIV''s 1714 edict admitting his legitimated sons to the succession within months of his death, while the Habsburg pragmatic sanction stood?',
    'Compare outcomes of attempted sovereign revisions across realms: registration practice in the French parlements, estate ratification in the Habsburg lands, and whether any attempted revision survived its author.',
    'If competence is general, the agenda-setting seat genuinely holds arbitrage-grade exit and the arrangement leans coordination; if competence is illusory outside exceptional cases, enforcement outruns the sovereign''s own control and the arrangement leans toward suppression without a working release valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revocability_vs_fundamental_law, empirical, 'Whether the sovereign override capacity this reading asserts was practically exercisable.').

omega_variable(
    renunciation_irrevocability,
    'Are dynastic renunciations (the Utrecht renunciations of 1713, by which claimants through Spanish princesses waived their rights) irrevocable acts, or are they themselves revocable instruments?',
    'Doctrinal analysis of renunciation theory plus the behavioral test: Philip V''s subsequent efforts to revive his line''s French claims, and whether other courts treated the waivers as extinguished or as dormant.',
    'If renunciations reliably extinguish claims, exit improves for female_line_claimants and measured suppression falls; if they are perpetually contestable, no reliable exit exists for any cognatic seat and suppression_requirement stays ratcheted high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renunciation_irrevocability, empirical, 'Whether renunciation offers a genuine exit channel from the prohibition.').

omega_variable(
    estate_ratification_depth,
    'Did estate ratifications of the pragmatic sanction reflect genuine consent to the settlement, or ratification under duress dressed as assent?',
    'Examine the terms extracted at ratification (the Hungarian estates traded acceptance for confirmed liberties), the alternatives available to refusing estates, and post-ratification compliance behavior during the succession war.',
    'Deep consent supports the coordination-function half of the tangled-rope structure; coerced ratification shifts weight toward the extraction half and raises effective suppression on the estate seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(estate_ratification_depth, empirical, 'Whether multi-estate acceptance of the settlement was consensual coordination or compelled assent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sali_tr_t5, salic_prohibition__sovereign_override_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__sovereign_override_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(sali_tr_t15, salic_prohibition__sovereign_override_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__sovereign_override_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(sali_tr_t25, salic_prohibition__sovereign_override_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__sovereign_override_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(sali_tr_t35, salic_prohibition__sovereign_override_reading, theater_ratio, 35, 0.22).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(sali_be_t5, salic_prohibition__sovereign_override_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__sovereign_override_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(sali_be_t15, salic_prohibition__sovereign_override_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__sovereign_override_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(sali_be_t25, salic_prohibition__sovereign_override_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__sovereign_override_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(sali_be_t35, salic_prohibition__sovereign_override_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(sali_su_t5, salic_prohibition__sovereign_override_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__sovereign_override_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(sali_su_t15, salic_prohibition__sovereign_override_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__sovereign_override_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(sali_su_t25, salic_prohibition__sovereign_override_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__sovereign_override_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(sali_su_t35, salic_prohibition__sovereign_override_reading, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Salic Law' covers three structurally distinct claims about one referent, decomposed per the eps-invariance principle into three linked stories. salic_prohibition__immutable_mandate_reading (claimed natural/divine law; low epsilon; mountain-trending) -> salic_prohibition__sovereign_override_reading (this story; claimed revocable positive law; middle epsilon; tangled rope) <- salic_prohibition__cognatic_reversion_reading (claimed never-valid import; high epsilon; snare-trending). The upstream immutable story is typically cited as evidence BY this reading's opponents and BY the cognatic reading's proponents alike; each member links the other two via affects_constraints. Eps values differ because each reading assesses the same standing arrangement through a different account of what kind of thing the prohibition is — not because any story measures a different arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

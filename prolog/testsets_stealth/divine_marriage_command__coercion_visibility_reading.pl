% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Post-Manifesto Monogamy Settlement (Coercion-Visibility Reading)
 *   domain: religious_authority/political_theology
 *
 * SUMMARY:
 *   In 1890 the LDS First Presidency issued the Manifesto suspending plural
 *   marriage under imminent federal destruction of the corporate church. This
 *   story instantiates ONE reading of the contested kernel 'divine marriage
 *   command': the coercion-visibility reading, on which the Manifesto is an
 *   acknowledged response to federal coercion and the doctrinal shift's
 *   legitimacy derives from institutional survival necessity — non-revelatory
 *   ground, admitted as such. The epsilon referent is the standing
 *   arrangement under contest: the post-1890 ecclesiastical monogamy regime
 *   (the ban, its discipline machinery, and the sustaining obligations
 *   attached to it), assessed by this reading's own lights. On those lights
 *   the arrangement carries a substantial manufactured-consent component —
 *   members assent under a revelatory framing whose operative ground is
 *   coercion — riding on top of a genuine survival coordination function.
 *   That hybrid structure is why the claimed type is tangled_rope: real
 *   coordination, asymmetric extraction, active enforcement. The claim and
 *   the metrics are authored independently; the engine computes per-seat
 *   classifications from the structural data. Family note: this is one of
 *   three linked stories decomposing the colloquial label 'the Manifesto' per
 *   the epsilon-invariance principle — the continuationist reading
 *   (prudential suspension, command intact) authors a lower epsilon with a
 *   narrower victim set; the substitutionist reading (superseding revelation)
 *   removes the manufactured-consent component entirely and relocates the
 *   extraction question. Each is a separate file with its own epsilon,
 *   beneficiaries, and victims; they are linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - lds_first_presidency: Agenda-setting beneficiary (institutional/identity_locked) — issued and administers the settlement; collects institutional survival and retained authority; bears the legitimacy exposure the reading names.
 *   - us_federal_government: External beneficiary with secondary agenda-setting role (institutional/arbitrage) — supplied the coercive force; collected the policy outcome; escalates or relaxes enforcement at will.
 *   - ordinary_latter_day_saint_members: Primary payer with secondary beneficiary position (moderate/constrained) — surrender covenant expectations and sustain the shift under revelatory framing; collectively receive the survival dividend.
 *   - pre_manifesto_plural_families: Full target (powerless/identity_locked) — existing plural covenants frozen or dissolved; bear the direct covenantal cost of a decision made over their heads.
 *   - post_manifesto_polygamous_dissenters: Full target (powerless/identity_locked) — continued the practice under the old command; bear discipline, excommunication, and exclusion.
 *   - fundamentalist_successor_movements: Excluded voice (powerless/trapped) — the organized continuation of the suppressed alternative; absent from the deciding councils, arriving as schism decades later.
 *   - senate_smoot_committee: Observer (institutional/analytical) — external adversarial scrutiny whose hearings forced the Second Manifesto enforcement ratchet.
 *   - political_theology_observers: Analytical observer (analytical/analytical) — sees the full three-reading structure and the legitimacy question the acknowledged-coercion account raises.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.7).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Post-Manifesto Monogamy Settlement (Coercion-Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '84aa66a7-88b8-4c34-9f86-7978c6d2ca8a').
narrative_ontology:cs_kernel_codification('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', fixed_text).
narrative_ontology:cs_authority_grounding('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', extraction).
narrative_ontology:cs_interpretation_layer_present('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a').
narrative_ontology:cs_reading_relation('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_axiom('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', foundational, institutional_survival_legitimates_doctrinal_shift).
narrative_ontology:cs_axiom_status(institutional_survival_legitimates_doctrinal_shift, holdable).
narrative_ontology:cs_axiom_grounding('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', institutional_survival_legitimates_doctrinal_shift, instrumental).
narrative_ontology:cs_axiom('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', secondary, coercive_origin_requires_acknowledgment).
narrative_ontology:cs_axiom_status(coercive_origin_requires_acknowledgment, holdable).
narrative_ontology:cs_axiom_grounding('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', coercive_origin_requires_acknowledgment, deontological).
narrative_ontology:cs_reference_frame('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', coercion_acknowledged_monogamy_settlement).
narrative_ontology:cs_drift_state('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', contemporary_historical_transparency_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('84aa66a7-88b8-4c34-9f86-7978c6d2ca8a', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, lds_first_presidency).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, us_federal_government).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, ordinary_latter_day_saint_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, pre_manifesto_plural_families).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, post_manifesto_polygamous_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, ordinary_latter_day_saint_members).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, legal_supremacy_over_ecclesiastical_practice).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto under imminent disincorporation, property seizure, and imprisonment of its members, and has administered the resulting monogamy regime ever since through church discipline, temple-interview questions, and public reaffirmation. Publicly frames the shift in revelatory language while the operative ground, on this reading, is the survival calculus it acted on. Cannot abandon the institution without dissolving the office itself; its authority and its survival are the same thing.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, lds_first_presidency, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, lds_first_presidency, beneficiary).

% Supplied the coercive force — anti-bigamy statutes, the Edmunds-Tucker Act, disincorporation proceedings, electoral disenfranchisement — that made continuation untenable. Collected the policy outcome (cessation of plural marriage) without operating the church. Retains the ability to escalate or relax enforcement as political convenience dictates, which is precisely the lever the 1904 Senate investigation pulled.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, us_federal_government, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, us_federal_government, agenda_setter).

% Asked to surrender covenant expectations they had treated as eternal, and to sustain the shift as a matter of faith, while the operative ground of the shift was state coercion. Bear the epistemic cost of assenting under a framing that misattributes the decision's origin. Collectively receive the survival dividend: no member faces the prosecute-or-apostatize choice alone, and the community, its property, and its gathering persist. Leaving means exiting an entire social and sacramental world.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, ordinary_latter_day_saint_members, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, ordinary_latter_day_saint_members, beneficiary).

% Families sealed under the pre-1890 command whose existing plural covenants were frozen, dissolved in practice, or rendered unspeakable. Wives and children of plural households bore the direct covenantal cost of a decision made over their heads; their identity was constituted by the very covenants the settlement unwound, so exit was not a relocation but a self-dissolution.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, pre_manifesto_plural_families, payer,
    powerless, civilizational, identity_locked, regional).

% Members who continued contracting or living in plural marriages after 1890, treating the earlier command as still binding. Subjected to escalating discipline — probation, disfellowshipment, excommunication — culminating in the post-1904 purges that removed apostles from office. Their obedience ran to the command; the enforcement ran against them.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, post_manifesto_polygamous_dissenters, payer,
    powerless, civilizational, identity_locked, regional).

% The organized continuation of the suppressed alternative, crystallizing in the early twentieth century around the claim that the 1886 revelation to John Taylor remained in force and the Manifesto changed nothing doctrinally. Had no seat in the councils that produced the settlement; their objection arrives only decades later, from outside, as schism.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, fundamentalist_successor_movements, excluded,
    powerless, civilizational, trapped, regional).

% Senate panel investigating whether a sitting apostolic officer could hold federal office while his church tolerated post-Manifesto plural marriage. Its hearings (1904-1907) subjected the settlement's enforcement claims to adversarial examination and forced the Second Manifesto and the disciplining of continuing polygamists — external scrutiny functioning as the settlement's audit mechanism.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, senate_smoot_committee, observer,
    institutional, biographical, analytical, national).

% Scholars of religion, law, and political theology who examine the settlement as a case of doctrinal change under state pressure. See the full three-reading structure of the contested kernel and the legitimacy question the coercion-visibility account raises: what happens to an authority structure that admits non-revelatory grounds for doctrinal shift.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, political_theology_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, lds_first_presidency).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a genuine collective-action problem: a dispersed covenant community facing existential state pressure needed one coordinated cessation decision so that no member or family faced the prosecution-or-apostasy choice alone. The Manifesto centralized the surrender, ended serial prosecutions, preserved the corporate body and its property, and opened the path to amnesty and statehood.
% TRANSFER_FUNCTION: Moves covenant compliance and religious assent from believing members and plural families to the federal state's enforcement objective, and moves loyalty, labor, and continued deference from the membership to the institutional leadership whose survival the compliance purchases. Legitimacy capital is transferred from the revelatory frame to the survival frame.
% ABSENT_VOICES: Those whose covenants constituted the settlement's cost had no seat in the councils that produced it: the wives and children of existing plural sealings, the holders of the 1886 (John Taylor) revelation tradition, and the future fundamentalist communities. On this reading there is a deeper absence: the revelatory channel the authority structure claimed to consult was not the operative ground of the decision — the voice structurally excluded from the room was the one the framing invoked.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the modern institution would rearrange around it immediately: the monogamy norm is load-bearing for the church's legal position, its temple practice, its social respectability, and its membership composition; revocation would trigger schism, legal conflict, and membership rupture. In 1890 the dependence was starker still — without the settlement the corporate church faced disincorporation and its leadership prison. Arrangements at both ends of the interval depend on it.
% FOUNDING_PROBLEM: Imminent corporate destruction: so long as plural marriage persisted, the church faced disincorporation under the Edmunds-Tucker Act, seizure of its temples and property, imprisonment of its leadership, disfranchisement of its members, and the effective termination of the gathering.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the federal court record itself — the church's own 1891 brief to the Supreme Court recited the abandonment as compelled by law — and by Woodruff's private papers ('the whole United States government is against us'), on which the scholarly literature (Quinn, Hardy, Flake) uniformly draws. These sources attest both the coercive genealogy and the fact that the coercive episode ended (statehood 1896, amnesty, enforcement wind-down) while the arrangement persisted. The benefiting parties' public attestation — the revelatory framing of Official Declaration 1 — diverges from this record, and that divergence is itself signal.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the settlement's compliance is secured under a legitimacy framing that, on this reading, misattributes the decision's ground: members' assent is harvested for a policy whose actual warrant is survival, not revelation, and dissenters are cut off entirely. Suppression (0.70) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Suppression here is structural (discipline machinery, temple-interview gatekeeping, the standing federal backstop) layered over internalized elements (sustaining leaders as a salvific duty). Theater ratio (0.45) is substantial but not dominant: the survival function is real, yet a large share of maintenance activity is performative — the public revelatory framing, the 1890s period of public denial alongside private authorization of new marriages, and the Second Manifesto's reaffirmation staged under Senate scrutiny. Accessibility collapse is 0.55: alternatives never fully closed — Mexican and Canadian colonies, quiet private continuation, and eventually the fundamentalist schisms kept exits partly alive, which is exactly why this is not a snare. Resistance is 0.50: secret post-Manifesto marriages, resistance at the apostolic level (two apostles ultimately removed), and the later schismatic movements. The temporal series run on ONE shared grid (1890, 1894, 1898, 1902, 1904, 1907, 1910) with every tracked metric authored at every point. The suppression_requirement series is authored because enforcement CAPACITY is the dynamic being traced: it decays through the 1890s (federal pressure relaxes, secret marriages flourish, enforcement is nominal) and then ratchets hard after 1904 (Second Manifesto, excommunications, apostolic resignations) — a U-shape, not drift. Extractiveness climbs as the gap between framing and ground widens and the double-bookkeeping compounds, peaking at the Smoot-era crisis before easing slightly as the purge completes and the settlement consolidates.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data explains why. From the presidency's position the settlement is the coordination it built under duress — a rope-like act of collective rescue it continues to administer; its identity_locked exit (the office IS the institution) means it experiences no alternative from inside. From the dissenter seats the identical structure operates as enforced extraction: their covenant-obedience ran one way and the discipline ran the other, with no exit that preserved identity. The federal seat experiences neither — it collected its policy outcome and retains arbitrage over enforcement intensity. Rank-and-file members sit between: genuine survival benefit, real epistemic cost, constrained exit. Same institution, same rule, four different computed types — driven by differentiated exit options and directionalities, not by any difference in the rule itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency is the structural beneficiary (d near the beneficiary end): it collects survival, retained office, and property, and its secondary beneficiary role is genuine even accounting for the legitimacy exposure it carries — the exposure is a cost of the benefit, not a reversal of it. The federal government sits near-beneficiary (d low): it received compliance without operating anything, and its arbitrage-grade control of enforcement intensity keeps it near the subsidized end. Ordinary members derive a middling d from their dual declaration (payer with secondary beneficiary): real costs borne, real survival received. Plural families and post-Manifesto dissenters derive d near the full-target end: they bear the transfer with identity_locked exits, which pins them at the trapped-target end of the derivation. The fundamentalist movements are excluded rather than coordinated — their exclusion is part of what the enforcement machinery maintains. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct qualitative ordering for every seat, including the one subtle case (the presidency's legitimacy exposure), which shifts its costs without flipping its net direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — imminent corporate destruction under federal coercion — is dead: statehood arrived in 1896, amnesty followed, and the enforcement machinery wound down after the Smoot settlement. The arrangement persists. Authored honestly, that is founding_problem_status=dead paired with disappearance_verdict=world_rearranges, which routes the mismatch consumer to a capture/zombie flag cross-checked against the theater path — and the temporal data supports exactly that reading: theater_ratio peaks at the Second Manifesto (a reaffirmation staged for an external auditor) and the settlement's maintenance grows increasingly performative as the original cause recedes. The tangled_rope classification is what prevents mislabeling in both directions: calling this a snare erases the genuine collective-action rescue (the community really did face destruction, and the coordination really did save it); calling it a rope erases the manufactured consent (assent secured under a framing that misattributes the decision's ground) and the purged dissenters. Whether the arrangement decays further toward piton depends on the persistence mechanism resolved by omega post_threat_persistence_mechanism: inertia-dominance drifts it pitonward; genuine doctrinal integration stabilizes it. The identity-lock dynamics matter here twice over: the presidency's institutional identity fusion (the organization has become its function) explains why the agenda-setter cannot fix what it knows, and the dissenters' covenantal identity fusion explains why the targets could not exit without self-dissolution — break either frame and the classification moves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_ground_attribution,
    'Is the Manifesto''s operative ground federal coercion with survival-derived legitimacy (this reading), prudential suspension of an eternally valid command (continuationist reading), or superseding revelation instituting monogamy (substitutionist reading)?',
    'Documentary convergence: Woodruff''s private journals and correspondence, the church''s own 1891 Supreme Court brief reciting abandonment as compelled by law, contemporaneous apostolic correspondence — triangulated against the distinct predictions each sibling reading makes about the record.',
    'Ground attribution fixes epsilon and the victim set for the whole family: the coercion-ground reading yields manufactured-consent extraction across the whole membership; the suspension-ground reading lowers epsilon and narrows victims to dissenters; the revelation-ground reading removes the manufactured-consent component entirely and relocates the extraction question to the enforcement of the new requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_ground_attribution, empirical, 'Which sibling reading correctly attributes the Manifesto''s operative ground — the family''s master uncertainty.').

omega_variable(
    legitimacy_two_track_stability,
    'Can the authority structure stably operate a two-track legitimacy model — public revelatory framing over an operative survival calculus — or does acknowledged coercion necessarily propagate into a general legitimacy crisis?',
    'Longitudinal analysis of how the institution transmits the Manifesto''s origin across generations (curriculum, official essays, apologetic literature) correlated with member trust measures, convert retention, and the growth of the fundamentalist exit stream.',
    'If the two-track model is unstable, effective extraction rises as consent is increasingly manufactured under premises the audience knows to be false, and the arrangement drifts toward snare; if stable, the tangled_rope classification holds and the acknowledged-coercion input is absorbed without cascade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_two_track_stability, conceptual, 'Whether admitting coercion as valid doctrinal input destabilizes the authority structure or is containable as prudence.').

omega_variable(
    coercion_counterfactual_completeness,
    'Was federal coercion sufficient to determine the Manifesto regardless of any concurrent revelatory experience — is the counterfactual ''no coercion, no Manifesto'' secure?',
    'Timeline analysis of enforcement escalation (Edmunds-Tucker provisions, disincorporation and property-seizure proceedings, incarceration of leadership) set against the internal decision record of 1888-1890.',
    'If coercion was only partial, part of the measured extraction reflects a freely chosen doctrinal course and epsilon drops toward the substitutionist band; if coercion was determinative, epsilon sits at the high end of the tangled_rope band and the manufactured-consent component is maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_counterfactual_completeness, empirical, 'Completeness of the coercion explanation for the doctrinal shift.').

omega_variable(
    post_threat_persistence_mechanism,
    'After the founding coercive threat ended (roughly 1896-1907), what sustains the monogamy settlement — genuine doctrinal integration and identity formation, institutional inertia, or the latent legal constraint that persists in modern law?',
    'Counterfactual institutional analysis: would the modern institution revive plural marriage absent legal prohibition; comparative evidence from the fundamentalist communities that did revive it; analysis of whether the settlement now generates its own internal justification independent of the original cause.',
    'Inertia-dominance pushes the arrangement toward piton drift and confirms the mandatrophy mismatch; genuine doctrinal integration stabilizes it as a settled tangled_rope trending toward rope, with the founding-problem death becoming irrelevant to its maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_threat_persistence_mechanism, empirical, 'Persistence mechanism after the founding problem''s death — the mandatrophy-resolving uncertainty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement_basis(divi_tr_t1890, observed).
narrative_ontology:measurement(divi_tr_t1894, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1894, 0.42).
narrative_ontology:measurement_basis(divi_tr_t1894, observed).
narrative_ontology:measurement(divi_tr_t1898, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1898, 0.48).
narrative_ontology:measurement_basis(divi_tr_t1898, observed).
narrative_ontology:measurement(divi_tr_t1902, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1902, 0.52).
narrative_ontology:measurement_basis(divi_tr_t1902, observed).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1904, 0.58).
narrative_ontology:measurement_basis(divi_tr_t1904, observed).
narrative_ontology:measurement(divi_tr_t1907, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1907, 0.5).
narrative_ontology:measurement_basis(divi_tr_t1907, observed).
narrative_ontology:measurement(divi_tr_t1910, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1910, 0.45).
narrative_ontology:measurement_basis(divi_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement_basis(divi_be_t1890, observed).
narrative_ontology:measurement(divi_be_t1894, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1894, 0.62).
narrative_ontology:measurement_basis(divi_be_t1894, observed).
narrative_ontology:measurement(divi_be_t1898, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1898, 0.64).
narrative_ontology:measurement_basis(divi_be_t1898, observed).
narrative_ontology:measurement(divi_be_t1902, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1902, 0.67).
narrative_ontology:measurement_basis(divi_be_t1902, observed).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1904, 0.7).
narrative_ontology:measurement_basis(divi_be_t1904, observed).
narrative_ontology:measurement(divi_be_t1907, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1907, 0.71).
narrative_ontology:measurement_basis(divi_be_t1907, observed).
narrative_ontology:measurement(divi_be_t1910, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1910, 0.68).
narrative_ontology:measurement_basis(divi_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement_basis(divi_su_t1890, observed).
narrative_ontology:measurement(divi_su_t1894, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1894, 0.5).
narrative_ontology:measurement_basis(divi_su_t1894, observed).
narrative_ontology:measurement(divi_su_t1898, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1898, 0.45).
narrative_ontology:measurement_basis(divi_su_t1898, observed).
narrative_ontology:measurement(divi_su_t1902, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1902, 0.5).
narrative_ontology:measurement_basis(divi_su_t1902, observed).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1904, 0.62).
narrative_ontology:measurement_basis(divi_su_t1904, observed).
narrative_ontology:measurement(divi_su_t1907, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1907, 0.7).
narrative_ontology:measurement_basis(divi_su_t1907, observed).
narrative_ontology:measurement(divi_su_t1910, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1910, 0.7).
narrative_ontology:measurement_basis(divi_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the 1890 Manifesto' per the epsilon-invariance principle. The label conflates three structurally distinct claims about one kernel (divine_marriage_command): (1) this coercion-visibility reading — acknowledged response to federal coercion, legitimacy from survival necessity, epsilon 0.68 with a manufactured-consent extraction component spread across the whole membership; (2) continuationist_reading — prudential suspension under duress with the command still doctrinally valid, lower epsilon, victim set narrowed to dissenters; (3) substitutionist_reading — new revelation superseding the prior command, which removes the manufactured-consent component entirely and changes the beneficiary structure. Upstream/downstream: the documentary record (court filings, private papers) feeds all three, but each reading's epsilon is intrinsic to its own claim and stable within it. Every family member links the others via affects_constraints; orphaning any one would sever contamination-propagation analysis across the legitimacy-crisis question they share.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Categorical Renunciation — Strict Pacifist Reading ('Never Be Maintained')
 *   domain: constitutional law/security policy/institutional legitimacy
 *
 * SUMMARY:
 *   Article 9 of the postwar Japanese constitution renounces war and provides
 *   that 'land, sea, and air forces, as well as other war potential, will
 *   never be maintained.' This story instantiates the strict pacifist reading
 *   of that kernel: the categorical language is taken at face value as an
 *   absolute prohibition on any organized armed forces, defensive included,
 *   with no implied self-defense exception. Under this reading the constraint
 *   solved a real postwar coordination problem — making the path of
 *   remilitarization constitutionally unavailable after 1931-45 — while
 *   imposing concentrated costs: the security establishment operates in a
 *   permanent legality gray zone, service members serve under a
 *   constitutional cloud, and exposed peripheral communities bear risk
 *   without a national protective instrument, their security outsourced to
 *   the US alliance. Over the interval the reading's grip eroded: the
 *   Self-Defense Forces grew into a well-resourced force, the 2014 cabinet
 *   reinterpretation opened the door to collective self-defense, and defense
 *   spending moved toward levels the categorical text forbids — leaving the
 *   prohibition increasingly maintained in words while reversed in substance.
 *   CONSTRAINT FAMILY NOTE: per the epsilon-invariance principle this is one
 *   of three linked stories decomposing the colloquial label 'Article 9'; the
 *   siblings (inherent_right_reading, collective_self_defense_reading) are
 *   separate files with their own epsilon, victim sets, and classifications,
 *   linked via network.affects_constraints. CLAIM/METRIC INDEPENDENCE:
 *   claimed_type is authored from the structure I believe true (genuine
 *   coordination function plus asymmetric, actively enforced extraction =
 *   tangled_rope); the metrics are authored from the operation I believe
 *   descriptively true, including a high and rising theater ratio signaling
 *   drift toward inertial performance. The engine computes per-seat
 *   classifications from the structural data; the divergence between claim
 *   and computed seat types is the datum, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - strict_pacifist_movements: primary beneficiary (organized/identity_locked) — draws moral authority, litigation strategy, and coalition identity from the categorical text
 *   - peace_constitution_political_class: primary beneficiary (powerful/constrained) — converts the renunciation into durable electoral identity and postwar fiscal headroom
 *   - former_victim_nations: secondary beneficiary (institutional/mobile) — regional states holding the renunciation as a standing assurance against remilitarization
 *   - japanese_security_establishment: primary target (institutional/trapped) — responsible for defense their core instrument is declared impermissible
 *   - jsdf_service_members: primary target (moderate/constrained) — serve under an organization the reading declares categorically impermissible
 *   - exposed_peripheral_communities: primary target (powerless/trapped) — front-line populations bearing risk without a national protective instrument
 *   - us_alliance_managers: dual-positioned beneficiary/payer (institutional/mobile) — receive basing and predictability while absorbing the uncovered deterrence burden
 *   - future_japanese_generations: excluded seat (powerless/trapped) — inherit the architecture without having been party to any settlement
 *   - supreme_court_of_japan: analytical observer (institutional/analytical) — declines to settle the core question, keeping the interpretive contest open
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.7).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.6).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Categorical Renunciation — Strict Pacifist Reading ('Never Be Maintained')").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional law/security policy/institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'e2d4c1de-a345-4310-a363-1d0656ef7b90').
narrative_ontology:cs_kernel_codification('e2d4c1de-a345-4310-a363-1d0656ef7b90', fixed_text).
narrative_ontology:cs_authority_grounding('e2d4c1de-a345-4310-a363-1d0656ef7b90', lineage).
narrative_ontology:cs_interpretation_layer_present('e2d4c1de-a345-4310-a363-1d0656ef7b90').
narrative_ontology:cs_reading_relation('e2d4c1de-a345-4310-a363-1d0656ef7b90', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('e2d4c1de-a345-4310-a363-1d0656ef7b90', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('e2d4c1de-a345-4310-a363-1d0656ef7b90', foundational, armed_forces_categorically_impermissible).
narrative_ontology:cs_axiom_status(armed_forces_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('e2d4c1de-a345-4310-a363-1d0656ef7b90', armed_forces_categorically_impermissible, conventional).
narrative_ontology:cs_axiom('e2d4c1de-a345-4310-a363-1d0656ef7b90', secondary, self_defense_via_nonmilitary_means_sufficient).
narrative_ontology:cs_axiom_status(self_defense_via_nonmilitary_means_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('e2d4c1de-a345-4310-a363-1d0656ef7b90', self_defense_via_nonmilitary_means_sufficient, instrumental).
narrative_ontology:cs_reference_frame('e2d4c1de-a345-4310-a363-1d0656ef7b90', original_text_absolute_renunciation).
narrative_ontology:cs_drift_state('e2d4c1de-a345-4310-a363-1d0656ef7b90', contemporary_post_reinterpretation_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('e2d4c1de-a345-4310-a363-1d0656ef7b90', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, strict_pacifist_movements).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, peace_constitution_political_class).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, former_victim_nations).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_security_establishment).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, jsdf_service_members).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, exposed_peripheral_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, us_alliance_managers).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, us_alliance_managers).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, civilian_control_principle).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, regional_reassurance_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grassroots peace organizations, religious pacifist communities, and hibakusha-linked groups that organize around the unconditional renunciation. The categorical text anchors their moral authority, their litigation strategy, and their national voice; softening the text to permit defensive forces would dissolve the bright line they organize around. Leaving the commitment would mean abandoning the identity that binds their coalitions together.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, strict_pacifist_movements, beneficiary,
    organized, generational, identity_locked, national).

% Politicians and parties whose platforms defend the renunciation clauses. The arrangement supplies a durable electoral identity, blocks rivals' preferred defense policies, and freed postwar fiscal capacity for economic investment rather than rearmament. Their careers are built on defending the text; revisiting it risks their coalition's defining issue.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, peace_constitution_political_class, beneficiary,
    powerful, generational, constrained, national).

% States that experienced Japanese invasion and occupation. The unconditional renunciation functions as a standing assurance: it caps Japanese military capacity regardless of who governs, and they treat every loosening step — reinterpretation, export relaxations, budget growth — as a matter to protest diplomatically.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, former_victim_nations, beneficiary,
    institutional, generational, mobile, continental).

% Defense ministry officials, uniformed leadership, and security scholars responsible for national defense. Under the categorical reading their core function lacks constitutional legitimacy: they plan for threats they assess as growing while the instrument they judge necessary is declared impermissible, leaving them to operate inside a legality gray zone and to argue through budgets and guidelines rather than doctrine.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_security_establishment, payer,
    institutional, biographical, trapped, national).

% Personnel of the Self-Defense Forces. They train, deploy for disaster relief, and stand watch under an organization whose very existence the categorical reading declares impermissible; for decades this carried tangible costs — contested legal status, honorific ambiguity, court cases over duties and pensions. Exit means leaving a career and vocation; staying means serving under a constitutional cloud.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, jsdf_service_members, payer,
    moderate, biographical, constrained, national).

% Communities on the strategic front line — Okinawa foremost, plus western Kyushu and the southwest island chains. They host the heaviest consequences of the security arrangement while the categorical reading promises them no national armed protection of their own; their risk exposure is set by decisions taken elsewhere, and relocating home or territory is not an option.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, exposed_peripheral_communities, payer,
    powerless, biographical, trapped, regional).

% US policymakers and commanders managing the alliance. They receive basing access, strategic predictability, and a partner that will not independently destabilize the region; they simultaneously absorb the maritime and deterrence burden Japan's renunciation leaves uncovered, and periodically press for larger Japanese contributions.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, us_alliance_managers, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, us_alliance_managers, payer).

% People not yet born or not yet of age when the settlement and its interpretations were fixed. They inherit whichever security architecture results — the vulnerability of the categorical reading or the rushed catch-up of its reversal — without having been party to any of the choices.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, future_japanese_generations, excluded,
    powerless, generational, trapped, national).

% The judiciary asked to settle the text's meaning. It has repeatedly declined to rule on the core question, treating defense arrangements as political questions, which leaves the interpretive contest among the other seats unresolved and the categorical reading neither confirmed nor buried.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, supreme_court_of_japan, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, peace_constitution_political_class).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the postwar problem of preventing militarism's recurrence: it removes military buildup from domestic political competition, locks in civilian control, frees national resources for reconstruction and economic development, and provides neighboring states a credible standing assurance against remilitarization.
% TRANSFER_FUNCTION: Moves security responsibility from Japanese state institutions onto the US alliance framework; moves risk onto exposed peripheral communities and future generations; and moves political legitimacy and fiscal resources toward civilian priorities and the actors identified with the renunciation identity.
% ABSENT_VOICES: Future generations had no seat at the 1946 drafting (authored under occupation amid devastation) or at any subsequent interpretive settlement, yet inherit the full tail risk of either vulnerability or rushed reversal. Okinawan and southwestern-island communities bore the arrangement's heaviest consequences with minimal voice in its design. Neighboring states can object diplomatically but cannot vote on the text that assures them.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, Japan's security policy would rearrange immediately: the remilitarization debate would resolve rapidly toward buildup, alliance burden-sharing would be renegotiated from strength, neighboring states would respond with their own adjustments, and the pacifist political identity built on the text would collapse or reorganize around something else. Arrangements across at least four countries currently depend on the constraint holding.
% FOUNDING_PROBLEM: The catastrophic militarism of 1931-45: wars of aggression launched by an unaccountable military, total mobilization of society, atomic devastation, occupation and loss of sovereignty. The arrangement was built to make that path constitutionally unavailable — not merely discouraged, but removed from the menu.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality is corroborated from outside any beneficiary seat by the Tokyo tribunal record and the mainstream historiography of the 1931-45 period. Its current status is disputed along the same lines as the kernel contest: neighboring states' official diplomatic statements attest continued salience (the problem lives as long as remilitarization capacity could return), while Japanese security-policy literature attests transformation (the aggression appetite is gone; only legitimate defense is disabled). No seat outside the beneficiary set settles the dispute — which is itself the finding recorded here.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the categorical ban decouples entirely from threat conditions: as the environment hardened (North Korean missiles, Chinese naval expansion, Taiwan contingencies), the opportunity cost borne by security-dependent parties rose every decade, and the arrangement layered political rents (electoral identity, fiscal headroom, alliance free-riding) atop the original anti-militarism function. Suppression (0.60) is a raw structural property, unscaled by power or scope: it reflects Article 96's supermajority-plus-referendum entrenchment, decades of judicial avoidance, and normative stigmatization of rearmament advocacy — not violent coercion; the formal amendment path exists, which is why suppression sits mid-range rather than high. Accessibility collapse (0.62) is substantial but incomplete: once the categorical reading is accepted, the armed-forces alternative closes almost completely, but an interpretive bypass existed — the government never accepted the strict reading, and the 2014 reinterpretation route circumvented Article 96 entirely, which is why alternatives did not collapse to natural-law levels. Resistance (0.68) is high and organized: a cross-class revisionist coalition (right-wing movements, parts of the security establishment, governing-party majorities) has campaigned continuously since the 1950s and achieved partial success through reinterpretation rather than amendment — evidence that coalition formation among otherwise diffuse payers is possible and partially effective. Theater ratio (0.66) is high and rising: the prohibition is increasingly maintained as constitutional liturgy — Diet tributes, anniversary reaffirmations, euphemistic force nomenclature — while material policy runs the other way. All three metric series run on one shared time grid (1947-2024, eight points) so every metric is authored at every examined time point; the trajectories are monotonic except suppression_requirement, which peaks at the 2014-15 reinterpretation fight (peak enforcement effort against majority public disapproval) and eases slightly afterward as the revisionist position normalized and the remaining enforcers applied less effective pressure.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical text. From the pacifist movements' seat the arrangement is a sacred covenant — their ideological identity is fused with the renunciation (exit is unthinkable because their self-conception is constituted by it; if that frame broke, they would experience the constraint's relaxation as betrayal, not liberation). From the security establishment's seat the same text is an institutional cage: an organization that has 'become' its defensive function is told that function lacks legitimacy, producing chronic role strain rather than obedience. Service members experience it biographically — careers spent under a constitutional cloud. Exposed peripheral communities experience it as risk allocated from elsewhere. The political-class seat experiences it as an asset. The court, refusing to adjudicate, preserves the contest rather than resolving it. Same nominal sovereign framework, opposite lived constraints — the engine computes this divergence from power, exit, and directional position, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for strict_pacifist_movements (identity_locked exit pushes them deeper toward the subsidized end — they cannot arbitrage away their position, so the constraint's benefits bind to them), peace_constitution_political_class (constrained exit, but they administer the interpretive apparatus and harvest its legitimacy yields), and former_victim_nations (mobile exit — their benefit is real but revocable, sitting them nearer symmetric than the domestic beneficiaries). Victim declarations drive high directionality for japanese_security_establishment (trapped: the state's defenders cannot exit the state whose defense is foreclosed), jsdf_service_members (constrained: vocation-bound), and exposed_peripheral_communities (trapped and powerless: full-target position, nowhere to relocate). us_alliance_managers are genuinely dual-positioned — the derivation from their paired beneficiary/payer roles should land them mid-range, receiving predictable gains while paying the deterrence bill. Spatial scope amplifies effective extraction modestly: the constraint operates nationally with continental reverberations, and verification of compliance (what counts as 'war potential') grows harder as capabilities blur.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrence of the militarism that produced 1931-45 — has transformed rather than died: the appetite for wars of aggression is plausibly gone, but the constraint now also disables capacities (minimum defense, collective action) its framers' categorical language sweeps in. The R5 interview records this as founding_problem_status: contested, crossed with disappearance_verdict: world_rearranges — no zombie flag fires, but the measurement series shows why vigilance is warranted: theater_ratio climbing monotonically past 0.6 with extractiveness rising in parallel is the classic signature of a mandate outliving part of its function while performance expands to fill the gap. The tangled_rope classification earns its keep here in both directions: a pure-snare reading would erase the genuine, corroborated achievement — half a century without remilitarization, civilian control entrenched, regional reassurance sustained — and a pure-rope reading would erase the concentrated, named payers and the political rents harvested from their exposure. The hybrid category holds both truths: real coordination, real extraction, active enforcement required to sustain the asymmetry. If the residual_normative_force omega resolves toward pure performance, expect recomputation toward piton; if the categorical reading is authoritatively displaced by a sibling reading, this file's epsilon retires with it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_article_9,
    'Does the kernel text''s phrase ''never be maintained'' fix a categorical prohibition on any armed forces (this reading), or does it admit an inherent-right carve-out for minimum defensive capacity (the inherent_right_reading sibling)?',
    'An authoritative judicial ruling on the core constitutionality question, or a formal Article 96 amendment settling the text''s scope, would resolve which reading the kernel instantiates.',
    'Under a sibling reading the victim set shrinks to offensive-war capability only, the constraint moves toward rope-like minimum-deterrence coordination, and this file''s epsilon no longer applies — the epsilon here is indexed to the categorical reading alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indexicality_of_article_9, conceptual, 'Which reading of the Article 9 kernel the constitutional text itself instantiates.').

omega_variable(
    residual_normative_force_question,
    'Does the categorical prohibition retain independent binding force at the margins (conscription, nuclear armament, overseas combat), or does it now survive only as identity performance atop a substantively defunct arrangement?',
    'Observe behavior where enforcement has receded: whether defense expansion respects any categorical stops or only fiscal and political ones, and whether any policy option remains politically unthinkable because of the text rather than despite it.',
    'If the residue is purely theatrical, the constraint drifts piton-ward; if it still binds at identifiable margins, the tangled_rope classification with a genuine coordination function holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_normative_force_question, empirical, 'Whether the strict reading still does causal work or merely performs.').

omega_variable(
    structural_vs_internalized_hold,
    'Is the constraint''s hold on Japanese security policy structural (Article 96 entrenchment, judicial avoidance, alliance lock-in) or internalized (the peace constitution fused into national self-conception, making rearmament feel like becoming a different people)?',
    'Post-change trajectory analysis: if pacifist restraint and identity politics persist after legal barriers are removed or diluted, the internalized share is large; if behavior tracks the legal text closely, the hold is structural.',
    'An internalized component raises the constraint''s true suppression above the formal-legal measure and would slow any post-amendment rearrangement considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_hold, empirical, 'Structural versus internalized mechanism sustaining the renunciation norm.').

omega_variable(
    alliance_substitute_pricing,
    'Is dependence on the US alliance a genuine functional substitute for the prohibited self-defense capacity, or an unpriced risk transfer whose costs fall on exposed populations?',
    'Credibility stress-testing of extended deterrence under Taiwan or peninsular contingencies, plus burden-sharing negotiation outcomes revealing the alliance''s actual price.',
    'If the substitute is unreliable, the burden borne by exposed_peripheral_communities is understated, the effective victim set widens, and epsilon moves upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alliance_substitute_pricing, empirical, 'Reliability and true cost of the alliance-dependence substitute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art9_strict_pacifist_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.14).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t1947, observed).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.24).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t1960, observed).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1972, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1972, 0.33).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t1972, observed).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1985, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1985, 0.41).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t1985, observed).
narrative_ontology:measurement(art9_strict_pacifist_tr_t1995, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t1995, observed).
narrative_ontology:measurement(art9_strict_pacifist_tr_t2005, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2005, 0.54).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t2005, observed).
narrative_ontology:measurement(art9_strict_pacifist_tr_t2014, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2014, 0.61).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t2014, observed).
narrative_ontology:measurement(art9_strict_pacifist_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.66).
narrative_ontology:measurement_basis(art9_strict_pacifist_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(art9_strict_pacifist_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.42).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t1947, observed).
narrative_ontology:measurement(art9_strict_pacifist_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.47).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t1960, observed).
narrative_ontology:measurement(art9_strict_pacifist_be_t1972, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1972, 0.51).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t1972, observed).
narrative_ontology:measurement(art9_strict_pacifist_be_t1985, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t1985, observed).
narrative_ontology:measurement(art9_strict_pacifist_be_t1995, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t1995, observed).
narrative_ontology:measurement(art9_strict_pacifist_be_t2005, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t2005, observed).
narrative_ontology:measurement(art9_strict_pacifist_be_t2014, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2014, 0.67).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t2014, observed).
narrative_ontology:measurement(art9_strict_pacifist_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.7).
narrative_ontology:measurement_basis(art9_strict_pacifist_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(art9_strict_pacifist_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.34).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t1947, observed).
narrative_ontology:measurement(art9_strict_pacifist_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.44).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t1960, observed).
narrative_ontology:measurement(art9_strict_pacifist_su_t1972, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1972, 0.49).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t1972, observed).
narrative_ontology:measurement(art9_strict_pacifist_su_t1985, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t1985, observed).
narrative_ontology:measurement(art9_strict_pacifist_su_t1995, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t1995, observed).
narrative_ontology:measurement(art9_strict_pacifist_su_t2005, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t2005, observed).
narrative_ontology:measurement(art9_strict_pacifist_su_t2014, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2014, 0.65).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t2014, observed).
narrative_ontology:measurement(art9_strict_pacifist_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement_basis(art9_strict_pacifist_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'Article 9' per the epsilon-invariance principle. The natural-language concept covers three structurally distinct claims: (1) this strict_pacifist_reading — categorical prohibition, broadest victim set including security autonomy, highest epsilon; (2) inherent_right_reading — minimum defensive capacity preserved, rope-flavored coordination with narrow victims; (3) collective_self_defense_reading — allied defense permitted, converting pacifist-identity holders into the payer seat. The textual-baseline reading (this one) is upstream: sibling readings are constructed as departures from the categorical text, so this story's interpretive contest feeds the legitimacy conditions of both dependents. Each member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

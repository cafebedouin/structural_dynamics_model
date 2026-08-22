% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Dueling as Legitimate but Practically Abandoned Honor Mechanism (External-Cost Drop Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the drop_reading of the honor_violence_legitimacy
 *   kernel: dueling's incidence collapsed across the eighteenth to twentieth
 *   centuries, but — on this reading — its structural legitimacy as an
 *   honor-restoration mechanism was never withdrawn. What changed was the
 *   external cost environment: criminal prosecution, career penalties
 *   (military commissions, professional licensing), social censure from an
 *   expanding non-aristocratic public sphere, and eventually insurance and
 *   liability exposure made actually dueling ruinously expensive, while the
 *   code duello itself, the seconds' etiquette, and the underlying claim that
 *   armed combat can honorably settle an insult remained intact and
 *   thinkable. This is the 'rising price, stable legitimacy' story, distinct
 *   from the contraction_reading (where honor itself was redefined to exclude
 *   violence, making dueling not merely costly but unthinkable) and the
 *   composite_reading (which holds both mechanisms operated together). The
 *   theater_ratio rises steeply across the interval precisely because this
 *   reading predicts an increasing gap between maintained
 *   ceremonial/legitimating apparatus (codes, seconds, honor courts) and
 *   near-vanishing actual combat — the apparatus persists as performance of a
 *   live option rarely exercised.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_class: primary beneficiary — retains exclusive access to a legitimated honor-restoration mechanism
 *   - dueling_code_arbiters: administers the surviving apparatus, benefits from its continued relevance
 *   - would_be_duelists: bears the sharp end of rising external costs while the mechanism remains socially mandatory to consider
 *   - families_of_duelists: powerless payers with no voice in the challenge decision
 *   - state_legal_authorities: excluded driver of the cost escalation this reading credits with the decline
 *   - social_historians: analytical observers comparing this reading against its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.42).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.35).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling as Legitimate but Practically Abandoned Honor Mechanism (External-Cost Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '7286ddc0-2535-4dce-8da6-5d8fa61b5508').
narrative_ontology:cs_kernel_codification('7286ddc0-2535-4dce-8da6-5d8fa61b5508', distributed).
narrative_ontology:cs_authority_grounding('7286ddc0-2535-4dce-8da6-5d8fa61b5508', practice).
narrative_ontology:cs_interpretation_layer_present('7286ddc0-2535-4dce-8da6-5d8fa61b5508').
narrative_ontology:cs_reading_relation('7286ddc0-2535-4dce-8da6-5d8fa61b5508', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7286ddc0-2535-4dce-8da6-5d8fa61b5508', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('7286ddc0-2535-4dce-8da6-5d8fa61b5508', foundational, combat_remains_valid_honor_restoration_regardless_of_frequency).
narrative_ontology:cs_axiom_status(combat_remains_valid_honor_restoration_regardless_of_frequency, holdable).
narrative_ontology:cs_axiom_grounding('7286ddc0-2535-4dce-8da6-5d8fa61b5508', combat_remains_valid_honor_restoration_regardless_of_frequency, conventional).
narrative_ontology:cs_axiom('7286ddc0-2535-4dce-8da6-5d8fa61b5508', secondary, external_material_cost_is_causally_sufficient_to_explain_decline).
narrative_ontology:cs_axiom_status(external_material_cost_is_causally_sufficient_to_explain_decline, holdable).
narrative_ontology:cs_axiom_grounding('7286ddc0-2535-4dce-8da6-5d8fa61b5508', external_material_cost_is_causally_sufficient_to_explain_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('7286ddc0-2535-4dce-8da6-5d8fa61b5508', code_duello_honor_restoration_standard).
narrative_ontology:cs_drift_state('7286ddc0-2535-4dce-8da6-5d8fa61b5508', late_nineteenth_century_legal_criminalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7286ddc0-2535-4dce-8da6-5d8fa61b5508', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, aristocratic_honor_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, dueling_code_arbiters).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, would_be_duelists).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, families_of_duelists).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, code_duello_as_valid_honor_restoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continues to hold dueling as a live, legitimate resource for restoring reputation when insulted. The mechanism's continued conceptual availability preserves the class's monopoly on formalized honor-repair — nobody outside the class can duel with the same social recognition. They rarely need to actually fight because the credible threat, backed by legal tolerance and social codes, is usually enough.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, aristocratic_honor_class, beneficiary,
    powerful, generational, constrained, national).

% Seconds, code-duello authorities, and honor courts administer the rules that keep dueling procedurally legitimate. They adjudicate insults, negotiate settlements short of combat, and maintain the institutional apparatus (codes, seconds' etiquette, venues) even as actual fights become rare. Their continued relevance depends on the mechanism staying thinkable, not on it being frequently used.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, dueling_code_arbiters, agenda_setter,
    organized, generational, mobile, national).

% Individual gentlemen who receive or give insults face real pressure to accept a challenge or lose standing, even though rising external costs — criminal prosecution, social censure from non-aristocratic opinion, economic ruin from imprisonment or exile, and later insurance/military-career penalties — make actually fighting increasingly costly. They bear the risk of death or legal consequence while the surrounding class treats the option as a live, honorable choice.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, would_be_duelists, payer,
    moderate, biographical, constrained, regional).

% Wives, children, and dependents bear the economic and emotional cost when a duel proceeds and a principal is killed, wounded, imprisoned, or exiled. They have no voice in whether the challenge is accepted; the honor calculus that legitimizes dueling does not price their exposure.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, families_of_duelists, payer,
    powerless, biographical, trapped, local).

% Increasingly criminalize dueling and impose real external costs (prosecution, loss of commission, civil liability), which is the very mechanism this reading credits with driving the practice's decline. Yet the state's criminalization does not succeed in delegitimizing dueling within the honor class's own framework — it raises the price without dissolving the underlying legitimacy claim, so the state's voice shapes frequency but is excluded from the honor-class's internal legitimacy conversation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_legal_authorities, excluded,
    institutional, generational, constrained, national).

% Study the divergence between dueling's declared legitimacy and its declining incidence, comparing this reading (external cost suppresses frequency without touching legitimacy) against sibling readings that locate the decline in redefinition of honor itself.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, aristocratic_honor_class).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling-as-legitimate-option coordinates status competition among honor-class peers: it provides an agreed, rule-bound procedure for restoring reputation after insult, averting less predictable forms of retaliation (feuds, assassination, open brawling) by channeling conflict into a scripted, witnessed, honor-preserving ritual.
% TRANSFER_FUNCTION: The mechanism transfers reputational capital from the loser (or the fallen) to the survivor/winner within the honor class, while externalizing the real costs of death, injury, legal jeopardy, and family destitution onto the duelists themselves and their dependents — costs the honor calculus does not internalize.
% ABSENT_VOICES: State legal authorities, whose escalating criminal and civil penalties are the causal engine of this reading's claimed decline, are structurally excluded from the honor class's internal legitimacy conversation — their sanctions change the price of dueling without altering its status as a legitimate honor-restoration mechanism inside the code. Families of duelists have no standing in the challenge/acceptance decision at all.
% DISAPPEARANCE_RATIONALE: If dueling's legitimacy were formally withdrawn overnight (rather than merely priced out), the honor class disputes what would happen: arbiters and the aristocratic class itself maintain the mechanism would simply go dormant further, since it is already rarely invoked — the world has already substantially rearranged around external costs, not around any change in what counts as honorable. Others (historians, the excluded state authorities) contend the mechanism's mere conceptual survival still shapes who is perceived as brave or cowardly, so its formal disappearance would still register as a change in the honor economy.
% FOUNDING_PROBLEM: Dueling arose to provide a formalized, rule-bound alternative to unregulated blood feuds and arbitrary violence among the nobility — a way to settle questions of honor with agreed procedure, witnesses, and closure, averting escalating retaliatory violence between families and factions.
% FOUNDING_PROBLEM_CORROBORATION: Dueling-code arbiters and the aristocratic class attest the founding problem (unregulated honor violence) remains conceptually live even if rarely triggered, which is why the code is preserved rather than abolished. State legal authorities and social historians, corroborating from outside the honor class, attest the founding problem is functionally dead — modern legal and economic structures now absorb reputational disputes through courts, press, and social sanction — and that the code's survival is inertial legitimacy rather than genuine necessity.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the mechanism transfers real risk (death, ruin) from the honor class's status competition onto the individual duelist and his dependents, but this reading holds the coordination function (channeling conflict into rule-bound ritual) genuinely reduces net violence relative to unregulated feuding, so extraction is not severe. Suppression is comparatively low and stays low across the interval (topping at 0.35) — on this reading nothing internal to the honor code suppresses alternatives; it is external legal and social cost, not internal coercion, doing the suppressive work, which is exactly why suppression stays flat while theater_ratio climbs. Accessibility_collapse is low (0.3): alternatives to dueling (apology, mediation, simply declining) remain available and increasingly chosen — this reading is precisely the story of behavioral substitution without conceptual foreclosure. Resistance (0.4) reflects growing external pushback (legal prosecution, press condemnation) without the honor class's own resistance to giving up the mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic honor class and code arbiters sit near the beneficiary end: they retain a socially exclusive, legitimated status-repair mechanism whose upkeep costs them little once it is rarely invoked. Would-be duelists and their families sit near the target end: they bear the residual risk of a mechanism that remains socially expected even as its material costs (legal, financial, mortal) escalate. State legal authorities are excluded rather than beneficiary or victim — their sanctions are the causal engine of the frequency drop but do not register as legitimacy-conferring or legitimacy-stripping within the honor class's own framework, which is the structural core of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is a candidate mandatrophy case precisely because founding_problem_status is authored as contested rather than flatly dead: the honor class insists the founding problem (unregulated retaliatory violence) remains conceptually live, while outside corroborators (state authorities, historians) hold it functionally resolved by modern legal and social substitutes. Classifying this as piton rather than snare or rope captures that: no concentrated beneficiary is extracting large ongoing rents (frequency has collapsed), but the ceremonial apparatus persists past the point where its coordination function is doing real work — rising theater_ratio is the diagnostic signature, not a concentrated capturer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_survival_vs_redefinition,
    'Did dueling''s decline result from external costs pricing out an unchanged legitimacy claim (this reading), or from honor itself being redefined to exclude violence (contraction_reading), or both operating together (composite_reading)?',
    'Textual analysis of honor-code literature, dueling manuals, and honor-court records across the decline period: if the code duello''s own language and justification remain stable while incidence drops, this reading is supported; if the discourse itself shifts to characterize dueling as dishonorable or unmanly rather than merely risky, contraction_reading is supported; if both signals appear simultaneously in different sub-populations, composite_reading is supported.',
    'If contraction_reading is correct, this story''s core claim (legitimacy unchanged, only frequency dropped) is false, and the constraint should be reclassified as one where accessibility_collapse is much higher (the option becomes conceptually unavailable, not merely costly) — a stronger claim than this reading authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_survival_vs_redefinition, conceptual, 'Whether the kernel''s drop is explained by cost alone, redefinition alone, or both together — the central contest between the three sibling readings.').

omega_variable(
    code_survival_as_genuine_vs_vestigial,
    'Is the surviving code duello apparatus (seconds, honor courts, dueling etiquette) genuinely still capable of legitimating combat, or is it vestigial theater maintained by an honor class with no intention of actually using it?',
    'Compare rates of formal challenge issuance and acceptance (not just completed duels) across the interval — a genuinely live mechanism should show issued challenges even where combat is avoided through negotiated settlement; a purely vestigial one should show declining challenge issuance alongside declining combat.',
    'If challenges themselves become rare (not just completed duels), the piton classification is strongly reinforced — the theater_ratio''s rise is real institutional atrophy, not just measurement noise. If challenges remain common even as combat becomes rare, the mechanism is functioning as designed (negotiated settlement under threat) and closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(code_survival_as_genuine_vs_vestigial, empirical, 'Whether the persisting code duello apparatus retains functional legitimating capacity or is purely ceremonial residue.').

omega_variable(
    external_cost_source_attribution,
    'Which external cost mechanism did the most work in suppressing dueling frequency: state criminalization, military/professional career penalties, or shifting non-aristocratic public opinion delegitimizing the practice to a wider audience (which would itself blur into the contraction_reading)?',
    'Comparative legal-historical analysis across jurisdictions with different criminalization timelines and career-penalty regimes, controlling for public-opinion shifts.',
    'If public-opinion delegitimization (rather than pure legal/career cost) is the dominant driver, the sharp reading/contraction boundary this story depends on becomes harder to sustain, since shifting public opinion about what counts as honorable shades into redefinition of honor itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(external_cost_source_attribution, empirical, 'Whether the external costs driving decline are purely material/legal or already entangled with normative redefinition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__drop_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__drop_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__drop_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(hono_tr_t80, honor_violence_legitimacy__drop_reading, theater_ratio, 80, 0.54).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__drop_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__drop_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__drop_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__drop_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(hono_be_t80, honor_violence_legitimacy__drop_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__drop_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__drop_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__drop_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__drop_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(hono_su_t80, honor_violence_legitimacy__drop_reading, suppression_requirement, 80, 0.34).
narrative_ontology:measurement(hono_su_t100, honor_violence_legitimacy__drop_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Part of a three-story decomposition of the 'honor_violence_legitimacy' kernel per the ε-invariance principle: dueling's colloquial decline narrative conflates a cost-driven frequency drop (this story), a legitimacy-redefinition account (contraction_reading), and an overdetermined combination (composite_reading). Each carries its own ε and stakeholder structure rather than averaging across the contested mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Anti-Dueling Prohibition Regime over Residual Honor Culture (Drop Reading)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   This story instantiates the drop_reading of the
 *   honor_settlement_legitimacy kernel. The standing arrangement under
 *   contest is the anti-dueling prohibition regime, comprising criminal
 *   statutes, service regulations, and the surrounding delegitimation
 *   machinery, as it operated on residual honor culture across the interval
 *   mapped to roughly 1860 through 1920. On this reading the regime
 *   suppressed but never eliminated honor settlement: officer corps, academic
 *   fencing corps, and aristocratic enclaves kept the practice live as a
 *   fringe option, compelling permanent selective enforcement and opening a
 *   widening gap between the written ban and administered tolerance. The
 *   claim and the metrics are independent authored facts: claimed_type
 *   tangled_rope states this reading's structural verdict, a genuine
 *   coordination achievement carrying asymmetric extraction from
 *   identity-locked adherents, while the metric values describe observed
 *   operation without tuning toward that verdict. Sibling readings are
 *   separate constraints linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - residual_honor_culture_adherents: primary target (organized/identity_locked) — bears criminalization of the settlement their identity requires
 *   - student_corps_fencers: secondary target (organized/identity_locked) — bear legal exposure for tolerated ritual practice
 *   - state_legal_establishments: agenda setter and receipt seat (institutional/mobile) — administer the ban and accrue jurisdiction
 *   - military_disciplinary_authorities: dual-positioned enforcer-beneficiary (institutional/constrained) — gain disciplinary monopoly, staffed by honor members
 *   - commercial_professional_classes: primary beneficiary (powerful/mobile) — inherit status conferral as it leaves the honor economy
 *   - challenge_pressured_gentlemen: protected beneficiary with reputational costs (moderate/constrained)
 *   - non_elite_litigants: excluded voice (powerless/trapped) — subject to the asymmetric underside of selective enforcement
 *   - comparative_legal_sociologists: analytical observer — attributes causation across suppression, cognition, and attrition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.62).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.72).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Anti-Dueling Prohibition Regime over Residual Honor Culture (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '2c2c5d0d-bf55-49db-819b-7a1d8bd5adac').
narrative_ontology:cs_kernel_codification('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', distributed).
narrative_ontology:cs_authority_grounding('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', lineage).
narrative_ontology:cs_interpretation_layer_present('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac').
narrative_ontology:cs_reading_relation('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', foundational, suppression_does_not_dissolve_honor_obligation).
narrative_ontology:cs_axiom_status(suppression_does_not_dissolve_honor_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', suppression_does_not_dissolve_honor_obligation, empirically_contingent).
narrative_ontology:cs_axiom('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', secondary, niche_transmission_sustains_normative_repertoire).
narrative_ontology:cs_axiom_status(niche_transmission_sustains_normative_repertoire, holdable).
narrative_ontology:cs_axiom_grounding('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', niche_transmission_sustains_normative_repertoire, empirically_contingent).
narrative_ontology:cs_reference_frame('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', contained_live_honor_option).
narrative_ontology:cs_drift_state('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', interwar_niche_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('2c2c5d0d-bf55-49db-819b-7a1d8bd5adac', '2026-08-05T09:30:00Z').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, state_legal_establishments).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, commercial_professional_classes).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, challenge_pressured_gentlemen).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, residual_honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, student_corps_fencers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, military_disciplinary_authorities).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, challenge_pressured_gentlemen).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Officers, aristocrats, and corps members in enclaves where answering an insult or a challenge is constitutive of standing. The prohibition regime criminalizes the settlement their identity runs on: each unresolved affront now carries a choice between legal jeopardy and social death within their reference group. Leaving the frame means resigning commissions, quitting corps, and forfeiting the marriage-and-advancement economy that honor standing feeds; most absorb the legal risk or shift practice into tolerated forms rather than exit.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_culture_adherents, payer,
    organized, generational, identity_locked, regional).

% Members of academic fencing corps who practice bounded, consensual blade rituals whose facial scars function as portable status credentials. They operate in a semi-legal gray zone: prosecution is sporadic, university discipline intermittent, and the activity itself the price of belonging. Departure is formally free but forfeits the brotherhood's entire standing economy, so the legal exposure is borne rather than escaped.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, student_corps_fencers, payer,
    organized, biographical, identity_locked, regional).

% Legislatures, courts, and prosecutor offices that own the prohibition statutes and calibrate their application. Every dispute moved from the field of honor into the courtroom consolidates their jurisdiction and their claim to be the sole lawful settler of grievances. They face recurring docket trade-offs and periodically deprioritize elite cases, which is where the gap between the written ban and practiced tolerance opens.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_establishments, agenda_setter,
    institutional, generational, mobile, national).

% Commands that regulate dueling inside the services. Barring private settlement hands them a disciplinary monopoly: grievances route through the chain of command instead of around it. Yet the officer corps is staffed by the very honor culture being regulated, so enforcement habitually resolves into administrative exits such as quiet resignation or transfer rather than public trial, and the authorities are chronically ambivalent about their own rule.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, military_disciplinary_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, military_disciplinary_authorities, beneficiary).

% Bourgeois merchants, lawyers, and professionals whose advancement runs on markets, credentials, and credit rather than honor standing. As status conferral migrates to their channels, they inherit the competitive ground the honor economy vacates and are insured against challenge pressure they were never well-armed to answer. Their contact with the regime is almost entirely as its silent beneficiaries.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, commercial_professional_classes, beneficiary,
    powerful, generational, mobile, national).

% Men inside honor-adjacent circles who do not wish to fight but cannot refuse a challenge without dishonor. The statute supplies the face-saving refusal that honor alone would not permit: the law forbids me. Their protection is real, but it is paid for in diminished standing among hard-line honor holders, and it exists only so long as the frame they are escaping continues to govern their circle.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, challenge_pressured_gentlemen, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, challenge_pressured_gentlemen, payer).

% Popular-class disputants who were never admitted to honor standing and so never had a settlement practice to lose. They meet the ordinary criminal law for their fights while watching elite affairs close with a resignation letter or a nominal fine. They would object to the class asymmetry of enforcement, but they hold no seat in the legislative chambers or corps councils where enforcement practice is actually set.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, non_elite_litigants, excluded,
    powerless, biographical, trapped, national).

% Researchers reconstructing the decline of the duel from court dockets, corps archives, regimental records, and press accounts. They weigh suppression, cognitive change, and demographic attrition against one another, publish the attributions, and hold no position in the honor economy they study.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, comparative_legal_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, state_legal_establishments).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The prohibition regime solves a genuine collective-action problem: it replaces decentralized private violence among armed elites, with its escalating feud and challenge spirals, with centralized public adjudication. It additionally solves a commitment problem inside honor groups by supplying every reluctant principal a face-saving exit from challenge obligations.
% TRANSFER_FUNCTION: It moves dispute-settlement authority from private honor-bearing individuals to state institutions; it moves status-conferral power from honor circles to market and professional channels; and it imposes compliance costs, meaning foregone honor satisfactions, legal exposure, and disciplinary risk, on the residual adherents who still inhabit the honor frame.
% ABSENT_VOICES: Non-elite litigants would object that the ban was enforced downward and tolerated upward: their comparable violence met the full criminal law while elite affairs closed administratively. The maimed and the kin of the killed in the fringe duels that continued under selective tolerance are also absent from every chamber that set enforcement policy.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reopen challenge spirals inside the surviving niches, force officer corps to rebuild internal grievance machinery, and partially remilitarize status competition in the regions where the frame persists; core commercial society would barely register the change, which is itself the asymmetry this reading documents.
% FOUNDING_PROBLEM: The regime was built to stop escalating private violence among armed elites: feud cycles, challenge spirals, and the state's inability to monopolize force in a stratum that answered insults with pistols.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: provincial court dockets and regimental disciplinary files show prosecutions and administrative actions continuing deep into the twentieth century, attesting that suppression work was still being done; parliamentary inquiry records and foreign observers documented both the persistence of the practice and the selectivity of its punishment. Honor-circle memoirs also attest the continuing pull of obligation, though they speak from inside the affected population. No party disputes that the founding problem dominated the core by the interval's end; the dispute is whether it died everywhere or survived at the niches.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends at 0.62 because the regime's burden concentrates on a shrinking but identity-locked population: as the coordination justification thins in core society, what remains is increasingly pure suppression of a minority practice, so the per-adherent burden rises even as the aggregate base contracts. Suppression is authored as a raw structural property at 0.72 and is deliberately left unscaled; the engine owns any directionality and scope scaling of extractiveness, not of suppression. Theater rises monotonically from 0.18 to 0.52, crossing the substitution threshold near the interval's end: the official performance of universal prohibition increasingly overlays a practiced regime of administrative exits, semi-legal corps fencing, and deprioritized dockets. The suppression_requirement series tracks enforcement capacity honestly because this story's subject is enforcement dynamics: the requirement climbs slowly rather than decaying, because demand for the suppressed option persists and every relaxation episode invited visible revival. Accessibility_collapse is low at 0.32, the drop_reading's signature: the honor-settlement alternative remains conceivable and operable inside the niches, in direct contrast to the contraction_reading's high-collapse profile. Resistance at 0.55 reflects sustained evasion, public defenses of the practice, and episodic revival movements. The three series share one time grid at decades 0 through 60 so the engine samples every metric at every point without scalar substitution. Dynamics are monotonic drift with revival blips rather than a cycle, so no oscillation mechanism is claimed; the blips are carried in the counterfactual omega.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently from the same structure. From the identity-locked adherents' position the regime is persecution of conscience: it criminalizes a duty they did not choose and offers exit only as self-betrayal. From the legal establishment's position the same structure is civilizational completion, the quiet triumph of the courtroom over the pistol ground. The military authorities sit between: they enforce the ban and profit from the disciplinary monopoly it creates, yet they are recruited from the honor culture it suppresses, so their enforcement practice systematically softens into resignation letters and transfers. Among nominal equals, civilian courts and service tribunals hold the same institutional power tier but diverge sharply in practice, differentiated by composition: the bench increasingly staffed from commercial-professional strata, the tribunals from the adherent population itself. Coalition potential among the payer seats is real but blunted: they are organized and elite, yet fragmented across corps, services, and regions, and their identity lock makes coordinated legal reform read as dishonor.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Residual honor adherents and corps fencers sit near the full-target end: victims with identity_locked exit, trapped between legal jeopardy and social death, so effective extraction lands on them at nearly full strength. State legal establishments derive near the beneficiary end as agenda setters collecting jurisdiction, and they are additionally the receipt seat: the regime's gains, meaning adjudication authority and enforcement legitimacy, demonstrably accrue to them. Commercial professional classes derive low as mobile beneficiaries. One override is declared: challenge_pressured_gentlemen would derive near-pure-beneficiary from their beneficiary role, but they pay reputational costs inside honor circles and their protection exists only while the suppressed frame governs their peers, so their true directional position is mixed at approximately 0.38 rather than the derived near-zero; the override is keyed to the moderate power atom, which maps uniquely to this seat in this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, escalating private violence among armed elites, is substantially solved across core commercial society, and everyone outside the honor niches agrees on that. Whether it is solved at the margin is precisely what the three readings dispute, hence founding_problem_status contested paired with disappearance_verdict world_rearranges: the arrangement still organizes real behavior somewhere, so it is not a zombie, but its mandate no longer covers the territory it administers. Mandatrophy is therefore not declared resolved. The classification guards against both mislabels: calling the regime a pure rope erases the criminals-by-law adherents whose identity it prosecutes; calling it a snare erases the genuine coordination achievement of replacing feud cycles with public adjudication, an achievement no serious historiography denies. The tangled_rope verdict holds both truths in one structure, and the rising theater series marks where the coordination half is decaying toward performance while the extraction half persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading (drop_reading) of the honor_settlement_legitimacy kernel: is the adherent''s remaining attachment to honor settlement held in place by external suppression of a live option, or has the option already dissolved cognitively as the contraction_reading holds?',
    'Compare enforcement dependence against attitudinal evidence: if prosecutions, corps tribunals, and expulsion threats are load-bearing (relaxation precedes resurgence), the drop_reading''s suppression-centered structure stands; if practice fades wherever enforcement lapses without revival, the contraction_reading''s cognitive-dissolution structure better fits and this file''s epsilon is overstated.',
    'Under the contraction_reading the same kernel instantiates a constraint with high accessibility_collapse and near-zero suppression requirement, computing closer to a self-sustaining rope; under this reading the regime carries permanent enforcement burden and identity-locked targets. The per-seat classifications diverge accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: which sibling reading of the honor-settlement kernel correctly locates the binding mechanism.').

omega_variable(
    niche_boundary_stability,
    'Are the geographic and social niches sustaining the honor repertoire (officer corps, student fencing corps, aristocratic enclaves) demographically closed and attrition-bound, or open to recruitment?',
    'Cohort analysis of academy commissions, corps membership rolls, and regional court dockets: falling intake with aging membership indicates closure; steady replacement indicates reproduction.',
    'Closed niches trend this arrangement toward a vestigial, inertia-maintained remainder; open niches lock the suppression requirement in permanently and stabilize the tangled structure with its asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(niche_boundary_stability, empirical, 'Whether the residual honor niches reproduce or merely decay.').

omega_variable(
    selective_enforcement_equity,
    'Was enforcement of the prohibition class-asymmetric: lenient administrative handling for elite affairs (quiet resignation, nominal sanction) alongside ordinary criminal punishment for comparable popular-class violence?',
    'Matched comparison of sentencing outcomes for duel-adjacent offenses versus common assault across class position, controlling for injury severity.',
    'A strong asymmetry raises the extraction borne by non-elite disputants who never chose the honor frame and pushes the enforcement apparatus toward a snare-flavored profile; parity supports the tangled_rope reading with extraction confined to consenting honor adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_equity, empirical, 'Whether the regime''s tolerance of fringe elite practice was purchased at popular classes'' expense.').

omega_variable(
    prohibition_lifting_counterfactual,
    'Would relaxing or repealing the prohibition have revived widespread dueling (proving suppression load-bearing), or would practice have stayed marginal (proving the option was already normatively inert)?',
    'Natural experiments: the interwar rehabilitation episodes in Italy and Hungary, post-war revival rhetoric in defeated officer cultures, and the semi-legal tolerance of academic fencing where prosecution was suspended.',
    'Observed revival confirms this reading''s structural delta (suppressed but not eliminated) and validates the elevated suppression_requirement series; absence of revival transfers weight to the contraction_reading and dates this reading''s obsolescence earlier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_lifting_counterfactual, empirical, 'Counterfactual test of whether the prohibition regime was doing real suppressive work at the margin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hono_tr_t10, honor_settlement_legitimacy__drop_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__drop_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__drop_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__drop_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(hono_tr_t50, honor_settlement_legitimacy__drop_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(hono_be_t10, honor_settlement_legitimacy__drop_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__drop_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__drop_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__drop_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(hono_be_t50, honor_settlement_legitimacy__drop_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(hono_su_t10, honor_settlement_legitimacy__drop_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__drop_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(hono_su_t30, honor_settlement_legitimacy__drop_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__drop_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(hono_su_t50, honor_settlement_legitimacy__drop_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__drop_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the death of dueling' per the epsilon-invariance principle. The label conflates three structurally distinct claims: contraction_reading (framework transformation made the duel cognitively unavailable, high accessibility collapse, minimal enforcement need), composite_reading (decline overdetermined across mechanisms, contraction-weighted), and this file, drop_reading (the option stayed live in niches and was held down by active suppression, low accessibility collapse, permanent enforcement burden). Each carries its own epsilon, victims, and classification; they are linked because the contraction claim, if established, drains this reading's suppression machinery of its load-bearing role, so purity degradation propagates along the edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, moderate, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

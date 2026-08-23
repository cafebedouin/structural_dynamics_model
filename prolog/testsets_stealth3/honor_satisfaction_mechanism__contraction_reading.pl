% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction Mechanism — Contraction Reading (Category-Level Evacuation of Dueling)
 *   domain: historical sociology/legal history/normative systems
 *
 * SUMMARY:
 *   The colloquial label the fall of dueling covers at least three
 *   structurally distinct claims, and per the epsilon-invariance principle
 *   they are authored as separate stories in one family. This file
 *   instantiates the contraction_reading: the claim that the
 *   honor-satisfaction constraint was not gradually suppressed, not merely
 *   reduced in frequency, and did not survive at the fringe — it was
 *   evacuated from the possibility space. By the interval's end, a European
 *   gentleman publicly insulted beyond endurance could no longer form demand
 *   satisfaction as a live option; the challenge letter, the seconds, the
 *   field at dawn survive only as period costume and sport with an explicit
 *   disclaiming frame. KEY AGENTS (by structural relationship):
 *   descendant_gentry_families — heirs of the formerly bound class
 *   ([organized]/[mobile]), now free;
 *   professional_classes_replacing_honor_economy — status competitors whose
 *   toolkit presupposes the closure ([organized]/[mobile]);
 *   state_judicial_institutions — absorbed the dispute jurisdiction, statutes
 *   dormant ([institutional]/[constrained]);
 *   military_officer_corps_traditionalists — last constituency for whom the
 *   code stayed thinkable ([organized]/[mobile]);
 *   heritage_performance_communities — stage the dead forms as spectacle
 *   ([moderate]/[mobile]); grievance_bearers_without_recourse_syntax —
 *   injured parties with no social syntax for a satisfaction claim, the
 *   excluded seat ([powerless]/[trapped]); comparative_norms_analysts —
 *   analytical observers adjudicating between the sibling accounts
 *   ([analytical]/[analytical]). EPSILON REFERENT: the standing arrangement
 *   under contest is the post-contraction settlement governing honor-dispute
 *   conduct, assessed by this reading's own lights; its epsilon is near-nil
 *   because the settlement imposes almost no costs on anyone. The historical
 *   regime's epsilon appears in the trajectory series (0.78 falling to 0.05),
 *   not in the scalar. CLAIM/METRIC INDEPENDENCE: claimed_type rope reflects
 *   my structural judgment that the standing settlement coordinates dispute
 *   conduct at negligible ongoing cost with no captured rents; the metrics
 *   (theater_ratio 0.85, accessibility_collapse 0.93) describe actual
 *   operation independently. Where computed classifications diverge from that
 *   claim, the divergence is the measurement.
 *
 * KEY AGENTS:
 *   - - descendant_gentry_families: Heirs of the class once bound to compulsory satisfaction — freed beneficiaries ([organized]/[mobile])
 *   - - professional_classes_replacing_honor_economy: Bourgeois-professional strata whose conflict toolkit presupposes the closure — beneficiaries ([organized]/[mobile])
 *   - - state_judicial_institutions: Courts and legislatures holding absorbed jurisdiction with dormant statutes — beneficiaries ([institutional]/[constrained])
 *   - - military_officer_corps_traditionalists: Last holders of the thinkable code; let it lapse within two generations — beneficiaries ([organized]/[mobile])
 *   - - heritage_performance_communities: Stage duel forms under explicit non-functional frames — beneficiaries ([moderate]/[mobile])
 *   - - grievance_bearers_without_recourse_syntax: Injured parties with no expressive syntax for satisfaction — excluded seat ([powerless]/[trapped])
 *   - - comparative_norms_analysts: Historians adjudicating decline, composite, and contraction accounts — analytical observers ([analytical]/[analytical])
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.03).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction Mechanism — Contraction Reading (Category-Level Evacuation of Dueling)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical sociology/legal history/normative systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '2b85d2d4-b64b-4ed8-936a-b7e9a88f1057').
narrative_ontology:cs_kernel_codification('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', fixed_text).
narrative_ontology:cs_authority_grounding('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', self_enforcing).
narrative_ontology:cs_reading_relation('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', foundational, possibility_space_evacuation_decisive).
narrative_ontology:cs_axiom_status(possibility_space_evacuation_decisive, holdable).
narrative_ontology:cs_axiom_grounding('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', possibility_space_evacuation_decisive, empirically_contingent).
narrative_ontology:cs_axiom('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', secondary, enforcement_accounts_insufficient_for_terminus).
narrative_ontology:cs_axiom_status(enforcement_accounts_insufficient_for_terminus, holdable).
narrative_ontology:cs_axiom_grounding('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', enforcement_accounts_insufficient_for_terminus, empirically_contingent).
narrative_ontology:cs_reference_frame('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', operative_dueling_settlement).
narrative_ontology:cs_drift_state('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', contemporary_post_contraction, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('2b85d2d4-b64b-4ed8-936a-b7e9a88f1057', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, descendant_gentry_families).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, professional_classes_replacing_honor_economy).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, state_judicial_institutions).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, heritage_performance_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, military_officer_corps_traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heirs of the landowning and officer families that once operated the satisfaction code. Family archives hold challenge letters, seconds' correspondence, and mourning notices from the era when a refused challenge meant permanent social ruin. Today the same families handle reputation conflicts through solicitors and press corrections and treat the archived challenges as heirlooms. Nothing binds them to any conduct code; they carry the memory without the obligation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, descendant_gentry_families, beneficiary,
    organized, generational, mobile, regional).

% Lawyers, journalists, academics, physicians, and merchants whose status competition displaced the honor economy. When defamed they issue corrections, litigate, or ignore the slight; their entire conflict toolkit presupposes that personal combat is not among the available moves, and they staff and consume the institutions that absorbed dispute resolution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, professional_classes_replacing_honor_economy, beneficiary,
    organized, biographical, mobile, global).

% Courts, prosecutors, and legislatures. Anti-dueling statutes sat on the books for generations with sparse enforcement; as the practice became inconceivable the statutes went dormant without ever needing repeal campaigns. The judiciary absorbed grievance-handling jurisdiction the satisfaction code once held without campaigning for it, and cannot relocate the monopoly it now exercises.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_judicial_institutions, beneficiary,
    institutional, generational, constrained, national).

% The last constituency for whom the code stayed thinkable: European and Latin American officer circles kept challenge etiquette alive into the mid-twentieth century, then let it lapse within two generations. Members of the transition generation describe relief at the end of obligatory satisfaction alongside nostalgia for the camaraderie rites surrounding it. Having left the honor frame themselves, nothing restrains them.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, military_officer_corps_traditionalists, beneficiary,
    organized, biographical, mobile, continental).

% Historical martial artists, period-drama producers, reenactors, and museum educators who stage duel forms for audiences. Every staging carries an explicit frame — a demonstration of how it was done, never a proposal for how a slight is answered. The performances reference the dead code, sustain no living obligation, and are consumed as spectacle or sport.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, heritage_performance_communities, beneficiary,
    moderate, biographical, mobile, global).

% People whose reputation or dignity suffers injuries that available remedies feel unequal to — a public slander too small to litigate profitably, an institutional humiliation with no hearing. In an earlier frame such a person might have demanded satisfaction; the phrase and the practice behind it have no place in their conceptual repertoire, so the grievance routes into complaint channels that often close without resolution. They are not consulted when dispute norms are designed or revised.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, grievance_bearers_without_recourse_syntax, excluded,
    powerless, immediate, trapped, national).

% Historians and sociologists of honor and violence who reconstruct how the satisfaction mechanism operated and why it ended, adjudicating between frequency-decline, multi-causal, and category-death accounts. Their analyses are the main external record of the transition and the seat from which this story is authored.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, comparative_norms_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bounded elite violence: converted open-ended feuds over status insults into single, rule-governed combats with negotiated satisfaction terms — apology, retractation, or exchanged fire — giving elites a credible, terminating dispute-resolution device in environments where central courts could not reliably protect reputation.
% TRANSFER_FUNCTION: Moved mortal risk and standing: principals delegated settlement authority to seconds and code protocols, and both principals bore calibrated death-risk as the price of a credible honor claim; after the contraction, grievance-handling transfers instead to state courts, press rebuttal, and professional ethics organs.
% ABSENT_VOICES: Historically: servants, women, and non-gentile classes, insulted routinely by the honor class yet barred from satisfaction, since the mechanism protected only gentlemen. Today: grievance-bearers whose injuries feel larger than any lawful remedy — they would demand recognition but possess no social syntax for a satisfaction claim; the excluded stakeholder seat records this absence.
% DISAPPEARANCE_RATIONALE: From the contraction seat: reopen the category and latent scripts — archived officer codes, romantic honor literature, challenge etiquette preserved in cultural memory — would partially reactivate, and the world rearranges. From the opposing assessment: the material bases (caste exclusivity, weak courts, closed-officer sociability) are irrecoverable, so a reopened category yields costume rather than combat, and the world stays roughly unchanged. The parties genuinely dispute which holds; the story leaves the verdict open pending the category_reopening_revival_test omega.
% FOUNDING_PROBLEM: Securing credible reputation defense for elites where central courts were weak or absent: an insult left unanswered marked a man permanently untrustworthy, so the satisfaction mechanism converted reputation-defense from endless feud into a terminating, rule-bound ordeal whose mortal stakes guaranteed the seriousness of every claim made through it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historiography of honor and violence — Nye's work on masculinity and male codes of honor, Frevert's studies of honor and dueling, McAleer's history of dueling — attests both the mechanism's weak-state credibility function and its obsolescence; the officer corps' own post-1918 abandonment testimonies record practitioners judging the founding problem dissolved. No party that benefits from the closure originated this genealogy.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).
:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Terminal extraction is 0.05: the standing settlement collects nothing and compels nothing; the residual figure registers the marginal expressive foreclosure discussed at the excluded seat. Suppression is 0.03 and is authored as the raw structural property it is — unscaled by power or scope; the closure needs no enforcement machinery, which is precisely the contraction reading's distinctive datum against the composite account. Accessibility_collapse is 0.93: unusually high for a non-mountain, because what has collapsed is not a menu of substitutes but the old option itself — the category is closed, and this value is the sharpest quantitative discriminator between this reading and the decline reading (where residual thinkability would hold collapse far lower). Resistance is 0.03: no organized revival movement exists; nostalgia without advocacy. Theater_ratio is 0.85 and rising across the series — every observable activity touching dueling today is performance, reenactment, or sport under an explicit disclaiming frame. This is authored as an honest descriptive fact while denying the piton inference it superficially invites: the performances sustain nothing. They are the cultural echo of a dead regime, not maintenance of the closure, and there is no administrator for whom a fix-cost asymmetry could obtain because no one administers the closure at all. The trajectory is monotone collapse with two war-driven inflections (pre-1914 erosion under bourgeois and legal pressure; 1914-1918 demolition of the social world that needed the code), completing by century's end — no oscillation, hence no intermittent-reinforcement reading and a single shared nine-point grid for all three tracked metrics. Identity-lock note: the historical regime held its principals identity_locked (honor fused with selfhood; refusal meant integrity-death), and the transition broke that fusion; the closure's current subjects are not locked at all — nothing binds them, which is why their exit atom is mobile.
 *
 * PERSPECTIVAL GAP:
 *   The same word dueling means coercion to the 1880 principal (a near-full-target seat: compelled risk, identity-locked exit, ruinous refusal), absence to the 2000 professional (a near-beneficiary seat: the trap dissolved before they were born), and foreclosure to the wronged party who senses an injury larger than any remedy but possesses no sentence in which to demand more. The engine computes per-seat classifications from the structural data; this story's arc makes the historical seats visible through the measurement series rather than through separate perspective objects, and the divergence between the 1880 seat's experience (a snare-shaped regime) and the 2000 seats' experience (a settlement) is carried entirely by the trajectory, not by reconciling the scalar to it.
 *
 * DIRECTIONALITY LOGIC:
 *   All four declared beneficiaries derive low directionality — the settlement subsidizes them in the weak sense of costing them nothing while preserving an order they operate within; none collects rents from it, so their chi is damped toward zero or negative. No victims are declared because no group bears material costs: the expressive foreclosure at the excluded seat is real but below the structural-victim threshold, and it is recorded there rather than inflated into a victims entry. The one override covers the powerless excluded seat: the derivation chain has no beneficiary or victim declaration to read for an excluded agent and would fall back to an arbitrary canonical value, so 0.55 encodes its assessed position — near-symmetric with a slight tilt toward bearing the closure's one residual cost. Global spatial scope mildly amplifies effective extraction through verification difficulty, but with a 0.05 base the product stays negligible. Suppression is not scaled: 0.03 is the structural fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline guards in three directions here. Against snare: no victims, no collection, nothing coerced — the transfer function that once moved mortal risk between principals has no current analogue. Against piton: the high theater_ratio is echo, not maintenance — a piton requires an administrator who bears a fix-cost asymmetry, and the contraction reading's core claim is that no administrator exists; the settlement holds because deviation is inconceivable, which is a different persistence mechanism from institutional inertia. Against false mountain: emerges_naturally is false, keeping the contingent, datable, reconstructible character of the closure on record despite its necessity-feeling presentation, with the ambiguity routed to the constructed_settlement_natural_presentation omega. On the genealogy side, the founding problem died with the function — the weak-state credibility gap that made mortal stakes rational was closed by centralized courts, credit systems, and mass-media correction — so there is no zombie mandate: the mismatch consumer should find status dead paired with verdict contested, not world_rearranges, and no capture flag is expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_settlement_natural_presentation,
    'Is the category-closure''s felt naturalness (that one simply cannot duel) a contingent historical achievement presenting as necessity, or does it reflect a genuine structural attractor in dispute-norm space?',
    'Comparative ethnography and history of honor systems with live satisfaction mechanisms in societies lacking the settlement; perturbation analysis of whether the closure regenerates after localized breaches.',
    'If constructed, the closure is revisable and the rope/settlement classification holds with revision-risk flagged; if a genuine attractor, a mountain-like fixity claim strengthens despite emerges_naturally being false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_settlement_natural_presentation, empirical, 'Whether the closure''s necessity-feeling masks contingent historical construction.').

omega_variable(
    kernel_terminus_ontology_disagreement,
    'This story instantiates only the contraction reading of the honor_satisfaction_mechanism kernel: what structural facts would differ if the decline_reading or composite_reading were instantiated instead?',
    'Author the sibling stories and compare epsilon referents, victim sets, and enforcement metrics; the disagreement is located at the terminus ontology — fringe-persistence versus category-death versus multi-causal dismantling.',
    'Under the decline reading a residual live practice keeps epsilon nonzero and the option thinkable, drifting classification toward a residue-with-theater profile; under the composite reading enforcement and statute metrics become load-bearing and the closure is partly a maintained artifact. This file''s rope claim is conditional on the contraction premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_terminus_ontology_disagreement, conceptual, 'Committer-frame omega: sibling readings of the same kernel would produce different constraint structures.').

omega_variable(
    expressive_foreclosure_cost,
    'Does the closure impose welfare-relevant costs on grievance-bearers who lack any satisfaction-syntax, or is the post-closure remedy landscape strictly superior for everyone?',
    'Compare dispute-resolution outcomes across the interval boundary; study grievance persistence where legal remedies feel disproportionate to the injury.',
    'Real costs would populate the victims structure and strain the rope claim toward hybrid coordination/extraction; negligible costs leave the clean rope profile with diffuse gains standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expressive_foreclosure_cost, empirical, 'Materiality of the expressive foreclosure recorded at the excluded stakeholder seat.').

omega_variable(
    category_reopening_revival_test,
    'If the dueling category were legally and semantically reopened tomorrow, would latent honor structures reactivate (the world rearranges) or stay inert (the world is unchanged)?',
    'Track revival attempts in honor-retentive subcultures and in jurisdictions where anti-duel statutes have lapsed; measure uptake of challenge forms.',
    'Resolves the contested disappearance verdict: reactivation confirms the closure is load-bearing coordination; inertness confirms it is epiphenomenal superstructure over dead material conditions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_reopening_revival_test, empirical, 'Counterfactual test of whether the closure does structural work or merely records a finished transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(hono_tr_t15, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(hono_tr_t30, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(hono_tr_t45, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(hono_tr_t75, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 75, 0.68).
narrative_ontology:measurement(hono_tr_t90, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 90, 0.78).
narrative_ontology:measurement(hono_tr_t105, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 105, 0.82).
narrative_ontology:measurement(hono_tr_t120, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 120, 0.85).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(hono_be_t15, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(hono_be_t30, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(hono_be_t45, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 45, 0.41).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(hono_be_t75, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 75, 0.11).
narrative_ontology:measurement(hono_be_t90, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 90, 0.06).
narrative_ontology:measurement(hono_be_t105, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 105, 0.05).
narrative_ontology:measurement(hono_be_t120, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 120, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0, 0.84).
narrative_ontology:measurement(hono_su_t15, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(hono_su_t30, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(hono_su_t45, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 45, 0.48).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 60, 0.26).
narrative_ontology:measurement(hono_su_t75, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 75, 0.14).
narrative_ontology:measurement(hono_su_t90, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 90, 0.07).
narrative_ontology:measurement(hono_su_t105, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 105, 0.04).
narrative_ontology:measurement(hono_su_t120, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 120, 0.03).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label the fall of dueling, per the epsilon-invariance principle. The contraction reading (this file) authors the terminus ontology: the standing arrangement is a category-closure with near-nil epsilon, maximal accessibility collapse, and no enforcement. The decline reading authors a residual live practice with nonzero epsilon and partial accessibility collapse. The composite reading authors a multi-mechanism dismantling in which enforcement and statute metrics are load-bearing. The upstream stories (decline, composite) concern transition mechanics; this downstream story concerns the terminal state they fail to fully specify. Each file links the other two via affects_constraints; epsilon values differ across the family because the readings instantiate different constraints sharing one referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__contraction_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

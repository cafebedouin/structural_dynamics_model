% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause — Colorblind (Anticlassification) Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   The Fourteenth Amendment's equal protection guarantee is a contested
 *   kernel: one persisting textual commitment that contending parties read as
 *   incompatible constraints. This file instantiates the colorblind reading
 *   alone — the clause as a permanent prohibition on all governmental racial
 *   classifications, with individuals as the sole rights-bearers and no
 *   benign category of race use. The standing arrangement this story is about
 *   is the anticlassification doctrine now in force: judicial review that
 *   strikes down race-conscious governmental action, most consequentially the
 *   2023 termination of race-conscious admissions. Assessed by this reading's
 *   own lights, the arrangement is a formal, uniformly applied,
 *   non-rent-collecting rule: it transfers no resources to any administering
 *   seat, and its protection is available to every person identically. Its
 *   costs fall on identifiable seats — institutions that operated
 *   race-conscious programs and applicants who accessed them — and the
 *   reading's distinguishing move is to deny that those costs constitute the
 *   constraint's victimization: harm, on this reading, is located in
 *   violations of the rule, never in its operation. The sibling readings
 *   (remedial_reading, diversity_reading) are separate constraint stories
 *   sharing this referent and authoring different epsilon; they are linked
 *   through the network, not folded into this file.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter (institutional/constrained) — interprets and enforces the anticlassification rule through strict scrutiny; sets the doctrine's boundaries; cannot exit its own reviewing role
 *   - individual_rights_bearers: primary beneficiary (powerless/constrained) — hold the judicially enforceable immunity from being sorted, ranked, or allocated by race
 *   - applicants_disfavored_by_race_conscious_policies: organized beneficiary (organized/constrained) — won race-neutral evaluation through litigation; the concrete contemporary winners
 *   - historically_subordinated_applicants: cost-bearing seat (powerless/constrained) — lost the preference channel in 2023; retain the same individual immunity as everyone else
 *   - selective_universities: institutional cost-bearer (institutional/constrained) — dismantled race-conscious admissions and now manage proxy practices under continuing scrutiny
 *   - racial_minority_communities: dual-positioned (moderate/constrained) — protected against hostile classification by the same rule that foreclosed race-targeted remediation
 *   - state_governments: dual-positioned institutional actor (institutional/constrained) — their preference bans are validated while their own race-conscious options are closed in both political directions
 *   - legal_academy: analytical observer (analytical/analytical) — produces the competing genealogies and outcome studies both camps deploy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.14).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.55).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause — Colorblind (Anticlassification) Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '7359db66-49a3-4dbf-8908-76db0f98b254').
narrative_ontology:cs_kernel_codification('7359db66-49a3-4dbf-8908-76db0f98b254', fixed_text).
narrative_ontology:cs_authority_grounding('7359db66-49a3-4dbf-8908-76db0f98b254', lineage).
narrative_ontology:cs_interpretation_layer_present('7359db66-49a3-4dbf-8908-76db0f98b254').
narrative_ontology:cs_reading_relation('7359db66-49a3-4dbf-8908-76db0f98b254', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('7359db66-49a3-4dbf-8908-76db0f98b254', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('7359db66-49a3-4dbf-8908-76db0f98b254', foundational, all_racial_classifications_prohibited).
narrative_ontology:cs_axiom_status(all_racial_classifications_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('7359db66-49a3-4dbf-8908-76db0f98b254', all_racial_classifications_prohibited, deontological).
narrative_ontology:cs_axiom('7359db66-49a3-4dbf-8908-76db0f98b254', foundational, no_benign_classification_category).
narrative_ontology:cs_axiom_status(no_benign_classification_category, holdable).
narrative_ontology:cs_axiom_grounding('7359db66-49a3-4dbf-8908-76db0f98b254', no_benign_classification_category, deontological).
narrative_ontology:cs_reference_frame('7359db66-49a3-4dbf-8908-76db0f98b254', anticlassification_equal_citizenship_frame).
narrative_ontology:cs_drift_state('7359db66-49a3-4dbf-8908-76db0f98b254', post_sffa_doctrinal_settlement, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7359db66-49a3-4dbf-8908-76db0f98b254', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individual_rights_bearers).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, applicants_disfavored_by_race_conscious_policies).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, racial_minority_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, historically_subordinated_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, state_governments).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, historically_subordinated_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, selective_universities).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, racial_minority_communities).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the Fourteenth Amendment's equal protection guarantee through case-by-case review. Since 2023 it subjects every governmental use of racial categories to the most demanding review standard available and has terminated race-conscious admissions at Harvard and UNC. Sets the doctrine's boundaries through majority opinions; its authority rests on precedent chains reaching to Reconstruction. It cannot exit its own reviewing role; appointment and retirement cycles are its only turnover mechanism.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Every person within US jurisdiction holds a judicially enforceable guarantee that government will not sort, rank, or allocate by race. The guarantee is exercised mostly passively — it consists in what they are spared. Practical exit (renunciation, emigration) is theoretical; nearly everyone lives under the guarantee for life.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_rights_bearers, beneficiary,
    powerless, biographical, constrained, national).

% Applicants to selective universities who, under the prior race-conscious admissions schemes, faced lowered admission odds relative to race-neutral evaluation — in the recent litigation, Asian-American and white applicants organized through a litigation nonprofit. They now receive guaranteed race-neutral file evaluation and won the removal of the preference mechanism. Their leverage came from organization and the courts, not from numbers.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, applicants_disfavored_by_race_conscious_policies, beneficiary,
    organized, biographical, constrained, national).

% Applicants from groups whose present disadvantage traces in part to past state-enforced subordination. Under the current regime they are evaluated without racial preference, forgoing an admissions channel that operated from the late 1960s until 2023; they retain the same individual guarantee against hostile classification as everyone else. Their remaining avenues are race-neutral proxies (adversity essays, socioeconomic targeting) and political mobilization for doctrinal change.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historically_subordinated_applicants, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, historically_subordinated_applicants, beneficiary).

% Research universities that operated race-conscious admissions until 2023 and now run race-neutral processes: they dismantled preference mechanisms, redesigned essays and outreach, and manage litigation exposure under continuing scrutiny of their proxy practices. Institutional self-concept at several is fused with the diversity mission they can no longer pursue by racial means; compliance was nonetheless rapid and near-universal.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, selective_universities, payer,
    institutional, generational, constrained, national).

% Communities defined by shared racialized history. They hold the same individual guarantee against hostile governmental classification — historically the guarantee's most consequential protection — while no longer having access to race-targeted programs in admissions, contracting, and grant-making that operated under prior doctrine. Their organizations continue litigating and lobbying at the doctrine's margins.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, racial_minority_communities, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, racial_minority_communities, payer).

% State legislatures and electorates. Several states banned racial preferences by referendum or statute years before the national doctrine converged (California's Proposition 209, Michigan's Proposal 2); the current rule validates those bans statewide. The same governments are simultaneously barred from enacting race-conscious programs of their own, whichever direction their politics point.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, state_governments, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, state_governments, payer).

% Constitutional scholars across the interpretive spectrum. They produce the competing genealogies (original meaning of the 1866 debates versus Reconstruction practice), outcome studies of strict scrutiny, and proxy-effectiveness analyses that both camps deploy. No enforcement role; influence runs through clerkships, briefs, and nominations.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single civic rule for distributing governmental burdens and benefits without racial sorting: it prevents caste-forming classifications, prevents racial patronage competition among groups racing to capture the state for in-group advantage, and gives administrators a bright, administrable line that avoids case-by-case adjudication of which racial uses are virtuous.
% TRANSFER_FUNCTION: Moves classification discretion away from governmental institutions and toward individuals as an immunity: officials lose the power to allocate by race; persons gain an enforceable claim not to be allocated by race. Adjudicatory authority over where that boundary sits concentrates in the federal courts.
% ABSENT_VOICES: Communities harmed by facially neutral policies with racially disparate effects have no seat: the anticlassification frame hears explicit-classification claims only, so the bearers of neutral-practice harms object from outside the frame's own logic (Washington v. Davis, McCleskey). Defenders of the remedial reading are present but outvoted — dissenting opinions and the bar — rather than absent; the frame gives group-disparity evidence no hearing rather than silencing its holders.
% DISAPPEARANCE_RATIONALE: If the anticlassification rule vanished overnight, thousands of statutes, programs, and forms referencing race would face immediate challenge or reinstatement: admissions, public contracting, grant-making, districting, and scholarship programs would rearrange within a season, state preference bans would lose their federal anchor, and the settlement of the Civil War amendments' meaning would reopen — the arrangements of American public life are built on this rule's standing.
% FOUNDING_PROBLEM: Protecting newly emancipated citizens from state-enforced racial caste — the Black Codes and later Jim Crow regimes that made race a legally operative category of subordination.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction-era legislative history (the 39th Congress debates, the Freedmen's Bureau authorization, the 1866 and 1871 Acts) and mainstream Reconstruction historiography attest the founding problem from outside the modern beneficiary set: the guarantee was built against state-sponsored racial caste. Whether that problem remains live is disputed by extra-beneficiary sources on both sides — civil-rights historiography and the remedial camp attest living residues; the post-2023 bench majority and its scholarly allies attest the problem closed. No single outside source settles the status; the disagreement itself is corroborated by dueling Supreme Court opinions.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is rope because this reading's structure shows a genuine coordination function (preventing caste-forming classification and racial patronage competition, with a bright administrable line), broad net benefit across participants, and no seat collecting material rents — while the metrics are authored as descriptively true of the arrangement's actual operation, including features the reading itself would acknowledge as imperfections. Extractiveness is low (0.14) because the rule collects nothing and applies identically; it is not zero because the rule does remove an opportunity channel whose absence lands on identifiable seats, and because enforcement consumes real institutional resources. Suppression (0.55) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation — reflecting that an entire class of policy instruments is voided by constitutional fiat while substitute instruments (class-based, geographic, adversity-based) remain open, which is why it is substantial but not total. Accessibility collapse (0.58) reflects that the race-conscious route is closed in practice once the rule is understood, while interpretive alternatives persist at the level of doctrine. Resistance (0.60) reflects five decades of sustained litigation, dissent, and scholarly opposition. Theater (0.32) tracks the widening gap between strict scrutiny's nominal openness and its operational near-closure, plus the post-2023 performance of race-neutrality by institutions pursuing proxies. The temporal series run on one shared seven-point grid. Base extractiveness is intentionally near-flat: the reading holds the principle constant across eras, and the flat series is this reading's own indexical signature — a sibling-reading story over the same referent would author a different shape. Suppression_requirement is U-shaped because the story genuinely tracks enforcement-capacity change: massive enforcement against Southern resistance in 1954-64 (federal troops, marshals), collapse of external resistance thereafter, redirection of enforcement toward benign classifications at lower intensity, and a partial rebuild after 2023 to police proxy practices. Theater rises monotonically as the balancing-test form hardened into functional per-se review. No directionality overrides are authored: the derivation from declared beneficiary/payer positions and exit options reproduces the structural relationships without correction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setter seat, the arrangement is doctrine to administer: the judiciary experiences rule-application, not burden. From the beneficiary seats, the same rule is a shield — for individual_rights_bearers a lifetime immunity from racial sorting, for applicants_disfavored_by_race_conscious_policies a won guarantee of race-neutral evaluation. From the payer seats, the identical structure is a foreclosure: selective_universities lost an instrument their institutional identities were partly built around, and historically_subordinated_applicants bear the cost of a closed remedial channel for disadvantages they did not create. The dual-positioned seats show the sharpest internal divergence: racial_minority_communities hold the anti-caste insurance that historically mattered most while losing the contemporary remedial toolkit; state_governments have their preference bans validated and their own race-conscious options simultaneously sealed. The engine computes these per-seat classifications from the structural data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality toward the subsidized end: individual_rights_bearers (d near 0.0 — pure protection, no burden), applicants_disfavored_by_race_conscious_policies (near-full beneficiary, with organization-grade exit into further litigation). Payer positions drive toward the target end: selective_universities (institutional power but constrained exit — compliance was rapid and near-universal, indicating the constraint binds them effectively) and historically_subordinated_applicants (powerless, constrained — the least able to replace what the rule removed, sitting nearest the full-target end among the governed). The dual-positioned seats derive intermediate d: racial_minority_communities and state_governments hold offsetting beneficiary and payer relations to the same rule. The judiciary derives low-to-mid d as agenda-setter: it administers and absorbs no material charge, collecting authority rather than rents. Identity-lock was considered for the universities — several fused institutional self-concept with the diversity mission — but the speed and universality of post-2023 compliance shows the lock was weak; constrained, not identity_locked, is the honest exit atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-enforced racial caste — is declared solved by this reading and contested overall, so mandatrophy_resolved is deliberately left unset. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): no automatic zombie flag fires, correctly, because the arrangement's function is actively exercised rather than ritually maintained. Theater_ratio at 0.32 sits below the piton band, and the flat extractiveness series gives the accumulation trigger nothing to fire on — there is no rent layering onto the coordination function. The classification prevents mislabeling in both directions: it keeps the reading's genuine coordination achievement (an enforceable anti-caste immunity) from being scored as pure extraction by the payer seats' experience alone, and it keeps the reading's own low-extraction self-assessment from being mistaken for a verified summit — the per-seat computation preserves the payer-side foreclosure as measured divergence rather than reconciling it away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (colorblind_reading) of the equal_protection_clause kernel; the sibling readings (remedial_reading, diversity_reading) instantiate different constraints from the same text with different beneficiary/victim structures. Which reading governs is the master uncertainty for every metric in this file.',
    'Constitutional politics: court composition, amendment, or sustained doctrinal consolidation. Cross-file comparison of the three sibling stories sharing this referent.',
    'If the remedial reading governed, the beneficiary/victim structure inverts (non-remediating institutions become the cost-bearing seats) and epsilon for the standing arrangement is authored far higher; if the diversity reading governed, the structure becomes conditional-permissive. This file''s low epsilon is valid only under this reading''s lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: same kernel text, three incompatible constraint instantiations.').

omega_variable(
    disparate_impact_tolerance,
    'The anticlassification rule binds explicit racial classifications while tolerating facially neutral practices with racially disparate effects (Washington v. Davis, McCleskey). Is that asymmetry intrinsic to the constraint as this reading defines it, or an implementation artifact the reading itself would condemn?',
    'Doctrinal evolution: whether disparate-impact liability ever attaches under the equal protection guarantee itself, or whether the reading''s adherents push to extend strict review to neutral practices with racialized effects.',
    'If intrinsic, the constraint quietly subsidizes incumbent neutral structures and effective extraction is higher than the formal-rule assessment suggests; if artifact, epsilon stays low and the asymmetry is enforcement error, not constraint content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disparate_impact_tolerance, conceptual, 'Whether the benign-classification/invidious-practice asymmetry is constitutive or incidental.').

omega_variable(
    founding_meaning_genealogy,
    'Did the 39th Congress understand the equal protection guarantee as prohibiting all racial classifications (this reading''s genealogy) or as protecting newly freed citizens from caste, a purpose compatible with remedial classification?',
    'Archival and legal-historical scholarship on the 1866 debates, the Freedmen''s Bureau authorization, and early enforcement practice; adjudication in originalist scholarship rather than by the benefiting parties.',
    'Resolves which sibling reading is the kernel''s authentic inheritor; reshapes the legitimacy conditions of the remedial reading specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_meaning_genealogy, empirical, 'Contested original meaning of the anticlassification premise.').

omega_variable(
    strict_scrutiny_operational_rate,
    'Is strict scrutiny a genuine balancing test the constraint could occasionally fail, or a near-per-se bar administered in the theatrical form of a balancing test?',
    'Compile the outcome record: survival rate of governmental racial classifications under strict scrutiny across the case law.',
    'A near-zero survival rate confirms the theater_ratio trajectory authored here and indicates the constraint''s suppression is understated by its nominal form; a meaningful survival rate would lower theater and soften the suppression picture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_scrutiny_operational_rate, empirical, 'Calibration of the gap between strict scrutiny''s nominal openness and operational closure.').

omega_variable(
    permanence_vs_coalition,
    'The reading holds race to be never relevant — a permanent constraint. Is that permanence a structural property of the principle or contingent on the current judicial coalition?',
    'Observe doctrine across successive appointment cycles: whether the anticlassification rule survives composition change or reverts to the Grutter-era containment posture.',
    'If coalition-contingent, the constraint''s time_horizon is biographical-to-generational rather than civilizational, and the permanence claim in this reading''s axioms is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_vs_coalition, empirical, 'Whether the no-sunset character of the rule is durable or political.').

omega_variable(
    proxy_equivalence_question,
    'Post-2023 race-neutral proxies (adversity essays, socioeconomic and geographic targeting) — do they reproduce substantially race-correlated selection effects, making the constraint''s suppression of explicit classification partly futile, or do they genuinely re-basis selection onto class and circumstance?',
    'Admissions outcome studies comparing pre- and post-2023 cohort composition under proxy regimes; disclosure audits of proxy design intent.',
    'If proxies restore raced outcomes, the constraint''s practical effect is displacement rather than elimination, raising effective suppression cost without changing formal incidence; if they genuinely re-basis, the constraint achieved its stated transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_equivalence_question, empirical, 'Whether race-neutral substitutes are equivalent-in-effect or genuinely different.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_colorblind_tr_t1954, equal_protection_clause__colorblind_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(ep_colorblind_tr_t1964, equal_protection_clause__colorblind_reading, theater_ratio, 1964, 0.1).
narrative_ontology:measurement(ep_colorblind_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.17).
narrative_ontology:measurement(ep_colorblind_tr_t1995, equal_protection_clause__colorblind_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(ep_colorblind_tr_t2003, equal_protection_clause__colorblind_reading, theater_ratio, 2003, 0.29).
narrative_ontology:measurement(ep_colorblind_tr_t2016, equal_protection_clause__colorblind_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(ep_colorblind_tr_t2023, equal_protection_clause__colorblind_reading, theater_ratio, 2023, 0.32).

% Extraction over time
narrative_ontology:measurement(ep_colorblind_be_t1954, equal_protection_clause__colorblind_reading, base_extractiveness, 1954, 0.1).
narrative_ontology:measurement(ep_colorblind_be_t1964, equal_protection_clause__colorblind_reading, base_extractiveness, 1964, 0.11).
narrative_ontology:measurement(ep_colorblind_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.13).
narrative_ontology:measurement(ep_colorblind_be_t1995, equal_protection_clause__colorblind_reading, base_extractiveness, 1995, 0.14).
narrative_ontology:measurement(ep_colorblind_be_t2003, equal_protection_clause__colorblind_reading, base_extractiveness, 2003, 0.12).
narrative_ontology:measurement(ep_colorblind_be_t2016, equal_protection_clause__colorblind_reading, base_extractiveness, 2016, 0.13).
narrative_ontology:measurement(ep_colorblind_be_t2023, equal_protection_clause__colorblind_reading, base_extractiveness, 2023, 0.14).

% Suppression requirement over time
narrative_ontology:measurement(ep_colorblind_su_t1954, equal_protection_clause__colorblind_reading, suppression_requirement, 1954, 0.78).
narrative_ontology:measurement(ep_colorblind_su_t1964, equal_protection_clause__colorblind_reading, suppression_requirement, 1964, 0.7).
narrative_ontology:measurement(ep_colorblind_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(ep_colorblind_su_t1995, equal_protection_clause__colorblind_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(ep_colorblind_su_t2003, equal_protection_clause__colorblind_reading, suppression_requirement, 2003, 0.44).
narrative_ontology:measurement(ep_colorblind_su_t2016, equal_protection_clause__colorblind_reading, suppression_requirement, 2016, 0.47).
narrative_ontology:measurement(ep_colorblind_su_t2023, equal_protection_clause__colorblind_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'equal protection' decomposes into three structurally distinct constraints — one per reading of the kernel. This story (colorblind_reading) carries very low epsilon, individuals-only beneficiaries, and no victim set; the remedial_reading story inverts the structure (non-remediating institutions become cost-bearing seats; epsilon authored high by its lights); the diversity_reading story is conditional-permissive with a narrower beneficiary set. The colorblind reading is upstream in current doctrinal force — its 2023 consolidation is cited by its adherents as settling the kernel, which is precisely the upstream-cited-as-evidence pattern — while the sibling readings persist as live litigation and scholarship. All three files link one another through affects_constraints; epsilon differences across the family are reading-indexed values over a shared referent, not measurement noise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

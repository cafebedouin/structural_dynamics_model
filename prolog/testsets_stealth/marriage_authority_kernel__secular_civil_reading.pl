% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the marriage_authority_kernel: the
 *   claim that marriage and family-law authority derive from a secular civil
 *   code — the Special Marriage Act 1954 — grounded in constitutional
 *   individual rights, with civil courts adjudicating. The standing
 *   arrangement under contest is the SMA regime as it actually operates:
 *   civil officers, a mandatory thirty-day published notice, objection
 *   windows, and a self-selected user base dominated by couples with no
 *   personal-law alternative. The act delivers genuine coordination
 *   (religion-neutral recognition, gender-symmetrical divorce and maintenance
 *   architecture) while its notice machinery concentrates safety and privacy
 *   costs on the very population that most depends on it. KEY AGENTS (by
 *   structural relationship): interfaith_intercaste_couples — primary
 *   beneficiary with embedded payer position (moderate/constrained);
 *   notice_exposed_couples — primary target (powerless/trapped);
 *   officer_harassed_applicants — secondary target (powerless/trapped);
 *   women_using_civil_divorce_rights — secondary beneficiary
 *   (moderate/constrained); civil_registry_administration — agenda setter
 *   (institutional/arbitrage); natal_family_patriarchs and
 *   religious_community_leaderships — excluded actors (organized/mobile);
 *   constitutional_courts — analytical observer (institutional/analytical).
 *   Claim and metrics are authored independently: the claimed type reflects
 *   my structural judgment that the same administrative machinery both
 *   delivers recognition and imposes exposure, held together by active
 *   enforcement; the metrics describe the arrangement's actual operation
 *   without being tuned to any predicted engine verdict. Epsilon's referent
 *   is the SMA arrangement as it stands, assessed by this reading's own
 *   lights (individual-rights frame) — not the fully reformed arrangement the
 *   reading would endorse.
 *
 * KEY AGENTS:
 *   - interfaith_intercaste_couples: primary beneficiary with embedded payer position (moderate/constrained) — receives civil recognition, bears notice-mediated exposure
 *   - notice_exposed_couples: primary target (powerless/trapped) — bears the safety cost of the published-notice window
 *   - officer_harassed_applicants: secondary target (powerless/trapped) — bears discretionary obstruction costs
 *   - women_using_civil_divorce_rights: secondary beneficiary (moderate/constrained) — relies on symmetrical civil divorce and maintenance terms
 *   - civil_registry_administration: agenda setter (institutional/arbitrage) — administers the machinery, collects fees and registry data
 *   - natal_family_patriarchs: excluded actor (organized/mobile) — lost veto; the interference vector during the notice window
 *   - religious_community_leaderships: excluded actor (organized/mobile) — lost jurisdictional authority over members' marriages
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates the notice's constitutional status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.25).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '6e118a26-80fc-45b6-9292-f14d580cd21b').
narrative_ontology:cs_kernel_codification('6e118a26-80fc-45b6-9292-f14d580cd21b', fixed_text).
narrative_ontology:cs_authority_grounding('6e118a26-80fc-45b6-9292-f14d580cd21b', lineage).
narrative_ontology:cs_interpretation_layer_present('6e118a26-80fc-45b6-9292-f14d580cd21b').
narrative_ontology:cs_reading_relation('6e118a26-80fc-45b6-9292-f14d580cd21b', marriage_authority_kernel__hindu_codified_reading, influences).
narrative_ontology:cs_reading_relation('6e118a26-80fc-45b6-9292-f14d580cd21b', marriage_authority_kernel__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('6e118a26-80fc-45b6-9292-f14d580cd21b', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e118a26-80fc-45b6-9292-f14d580cd21b', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('6e118a26-80fc-45b6-9292-f14d580cd21b', foundational, marriage_authority_flows_from_individual_consent).
narrative_ontology:cs_axiom_status(marriage_authority_flows_from_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('6e118a26-80fc-45b6-9292-f14d580cd21b', marriage_authority_flows_from_individual_consent, deontological).
narrative_ontology:cs_axiom('6e118a26-80fc-45b6-9292-f14d580cd21b', secondary, civil_courts_hold_exclusive_family_jurisdiction).
narrative_ontology:cs_axiom_status(civil_courts_hold_exclusive_family_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('6e118a26-80fc-45b6-9292-f14d580cd21b', civil_courts_hold_exclusive_family_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('6e118a26-80fc-45b6-9292-f14d580cd21b', constitutional_individual_consent_marriage_order).
narrative_ontology:cs_drift_state('6e118a26-80fc-45b6-9292-f14d580cd21b', contemporary_post_safiya_sultana, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e118a26-80fc-45b6-9292-f14d580cd21b', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, interfaith_intercaste_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_using_civil_divorce_rights).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, civil_registry_administration).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, notice_exposed_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, officer_harassed_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, interfaith_intercaste_couples).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_individual_rights_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, individual_consent_marriage_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Marry across religious or caste lines and therefore have no personal-law route that will register their union without prior conversion. The Special Marriage Act is the only statute under which their marriage can be solemnized and registered as-is. They file notice with a marriage officer, wait out the thirty-day window, and appear for solemnization, paying nominal fees and supplying documentation. Civil recognition, monogamy defaults, and symmetrical divorce grounds flow to them. Abandoning the process midway forfeits the filing and restarts the clock; completing it can trigger family reprisal they never chose to publicize.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, interfaith_intercaste_couples, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, interfaith_intercaste_couples, payer).

% The subset of applicants whose thirty-day notice is read by someone hostile: a parent, a relative, a local office-bearer who forwards it. Because the notice is posted publicly at the marriage officer's office with names, addresses, and dates, their marriage plans become legible to the people most likely to obstruct them. Documented consequences range from family detention and pressured withdrawal to police complaints and, in extreme cases, violence. They are typically young, financially dependent on natal households, and already mid-procedure when exposure occurs, with no way to make a posted notice unread.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, notice_exposed_couples, payer,
    powerless, biographical, trapped, national).

% Applicants who encounter discretionary obstruction at the registration stage: officers delaying acknowledgment, demanding proof of age, residence, or conversion beyond the statutory checklist, summoning parents, or referring couples for police verification. Each demand is individually deniable and collectively costly; applicants lack a complaint channel that does not further expose them, and their timeline is hostage to the officer's calendar.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, officer_harassed_applicants, payer,
    powerless, biographical, trapped, national).

% Women who marry or dissolve marriages under the civil framework and rely on its symmetrical divorce grounds, monogamy guarantee, and maintenance provisions — terms several personal-law regimes grant less evenly. Their entitlement is mediated by courts and enforcement machinery; obtaining maintenance or interim support can take years, so the formal right and the lived outcome frequently diverge.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_using_civil_divorce_rights, beneficiary,
    moderate, biographical, constrained, national).

% Marriage officers and the registry bureaucracy that receive notices, publish them, hear objections, solemnize marriages, and maintain the register. They collect nominal fees and accumulate a uniform national dataset of civil marriages. They administer whichever marriage statutes parliament enacts and can shift emphasis between parallel regimes without sunk cost in any one of them.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_registry_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, civil_registry_administration, beneficiary).

% Parents and senior kin whose effective veto over adult children's marriages depends on learning of the marriage in time to stop it. The civil route removes their jurisdiction: no priest, council, or community body mediates consent. Their remaining lever is the publicity window — intercepting couples during the notice period through persuasion, pressure, or force. They are not parties to the statute and reject its individual-consent premises outright.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, natal_family_patriarchs, excluded,
    organized, biographical, mobile, regional).

% Clerical bodies and personal-law boards whose authority over members' marriages rests on community membership. A civil route any member may take without religious ceremony or permission competes with that authority at the margin. They argue for community jurisdiction and against the constitutional-individual framing; they hold no seat in the act's administration.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_leaderships, excluded,
    organized, generational, mobile, national).

% High Courts and the Supreme Court, which adjudicate challenges to the act's procedures — most prominently whether the thirty-day public notice survives the privacy and liberty guarantees for consenting adults. They hear petitions from affected couples and NGOs, issue rulings binding on officers, and can read down or strike provisions while leaving the statute's core intact.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, civil_registry_administration).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one religion-neutral legal form of marriage available to any two citizens: consent-based, monogamous, solemnized and registered by civil officers, with symmetrical divorce and maintenance grounds — solving the problem that inter-religious and inter-caste couples otherwise have no statute under which their union is recognized without prior conversion.
% TRANSFER_FUNCTION: Moves adjudicatory authority over marriage formation and dissolution from community and religious bodies to civil courts and officers; moves nominal fees and registry data from couples to the state; and, through the thirty-day published notice, temporarily transfers informational control over the marriage decision from the couple to whoever reads the notice.
% ABSENT_VOICES: Natal-family patriarchs and religious-community leaderships would object that marriage jurisdiction belongs to families and communities rather than individuals and civil officers; they are absent because the act's premise excludes their veto, not because they were consulted and overruled. Also absent: couples deterred before filing, who never enter the registration record, making their deterrence statistically invisible.
% DISAPPEARANCE_RATIONALE: Inter-religious and inter-caste couples would revert to conversion under duress or to unrecognized unions; existing civil marriages would lose their governing divorce and maintenance framework; the exit channel from personal law that courts and couples routinely use would close; and jurisdiction over marriage would snap back to community bodies by default.
% FOUNDING_PROBLEM: At independence India inherited religiously segmented family law with no legal form of marriage crossing community lines: an inter-religious couple had to convert to marry legally, and individuals wishing to leave community law had no civil exit. Parliament enacted the Special Marriage Act in 1954 to provide that religion-neutral form and, per the uniform-civil-code directive, to seed a common civil framework.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary debates accompanying the 1954 Act and successive Law Commission of India papers attest the deficit the act addresses; academic family-law histories corroborate the pre-1954 conversion dilemma. Religious leaderships deny the deficit, holding community law sufficient — so corroboration from outside the beneficiary set exists in the documentary and scholarly record but is disputed by the excluded seats.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.42: the act captures little wealth (nominal fees), but the published-notice design converts applicants' private decisions into publicly readable records, imposing concentrated safety, privacy, and timeline costs on a population that cannot route around the statute — a real, non-trivial extraction paid in security rather than money. Suppression 0.25 is authored as a raw structural property, unscaled by power or scope: the act suppresses almost nothing — personal-law alternatives remain fully open, and its main dampening effect is deterrence of its own prospective users. Theater_ratio 0.38: the fraud-prevention rationale for the objection window has thinned as civil verification improved, while the window's observed function has shifted toward enabling interference; core registration and solemnization remain functional. Accessibility_collapse 0.20: understanding the act collapses no alternatives — four personal-law regimes continue alongside it. Resistance 0.45: sustained constitutional litigation against the notice, conservative political opposition to the act's expansion, and family-level resistance to couples using it. The measurement series run on one shared eight-point grid (t=0..70, i.e., 1954-2024) so every tracked metric is authored at every examined time point; all points are observed. The gentle monotone rise in extractiveness tracks the hardening operating environment for inter-faith couples (organized opposition, officer notification practices) rather than any legislative change; the slow suppression_requirement rise models enforcement-capacity hardening at the officer level, capped by judicial pushback. Coalition note: the powerless seats' realistic power channel is coalition — NGO-backed constitutional litigation (the line of High Court rulings against the notice) is how notice-exposed couples have contested the procedure they cannot contest individually.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the trapped payer seats (notice_exposed_couples, officer_harassed_applicants), the arrangement operates as enforced exposure: a mandatory public ritual standing between them and legal recognition, administered by officials with discretion and no reciprocal accountability — the same structure reads close to pure extraction from where they stand. From the beneficiary seats (women_using_civil_divorce_rights, and interfaith couples who complete the process uneventfully), it operates as a genuine service no other statute provides. From the agenda-setter seat, it is routine administration. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for women_using_civil_divorce_rights and (net of their payer side) interfaith_intercaste_couples; victim declarations drive high directionality for notice_exposed_couples and officer_harassed_applicants, amplified by their trapped exit position — they are mid-procedure dependents of the very office exposing them. civil_registry_administration sits mid-range: it runs the machinery and collects fees and data, but its stake is administrative, not extractive rent. The excluded actors (natal_family_patriarchs, religious_community_leaderships) sit outside the beneficiary/victim derivation entirely — they are harmed by the arrangement's existence, not through it, and their opposition runs through social and political channels the statute does not govern. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: inter-religious marriage remains legally fraught, the conversion dilemma persists, and the national uniform code remains unrealized, so the arrangement has not outlived its mandate. The R5 mismatch consumer should find no zombie flag here — founding_problem_status=live combined with disappearance_verdict=world_rearranges is internally coherent. The classification work this story performs is bidirectional: it prevents the act's genuine coordination half (recognition, gender-symmetrical civil terms) from being mislabeled as pure extraction by the excluded seats' polemics, and it prevents the notice machinery from being excused as mere coordination overhead by the administration's procedural framing. Theater_ratio is authored honestly at 0.38 because the fraud-protection justification is partly performative — but theatricality is a symptom here, not the test; the structural test is who pays for the notice window and who could remove it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story instantiates only the secular_civil_reading of marriage_authority_kernel; what structural facts would differ if the standing arrangement were one of the sibling readings (hindu_codified, muslim_shariat, christian_canonical, parsi_communal)?',
    'Author each sibling as its own constraint story with its own statute, adjudicating bodies, and beneficiary/victim sets; compare computed types and epsilon across the family.',
    'Sibling arrangements are expected to show higher gender-asymmetric costs per the declared structural delta, shifting their classifications toward more extractive types; this reading''s values must never be averaged across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame decomposition: one kernel, five readings, five separate constraints with separate epsilon values.').

omega_variable(
    notice_necessity_separability,
    'Is the thirty-day published notice structurally necessary to the act''s fraud-prevention function, or is it a separable social-notification feature whose removal would leave the coordination function intact?',
    'Compare bigamy and fraud incidence, and objection-window utility, across periods or jurisdictions with confidential or shortened registration (judicial orders suspending notice; state-level reforms); if fraud detection does not degrade, the notice is separable.',
    'If separable, the arrangement''s extractiveness falls toward pure-coordination levels and its classification shifts toward rope; if inseparable, part of the measured cost is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notice_necessity_separability, empirical, 'Whether the notice machinery is load-bearing for the act''s coordination function or removable social-control residue.').

omega_variable(
    adverse_selection_generalizability,
    'The act''s users are overwhelmingly couples with no personal-law alternative; do the measured harms (notice exposure, officer obstruction) characterize the arrangement itself, or the composition of its self-selected user base?',
    'Compare outcome distributions for voluntary same-community registrants against inter-faith and inter-caste registrants under the same statute.',
    'If harms concentrate in the no-alternative subgroup, the effective burden is context-amplified rather than intrinsic, and reform priority shifts from the statute''s text to its operating environment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adverse_selection_generalizability, empirical, 'Whether measured harms are intrinsic to the arrangement or artifacts of who uses it.').

omega_variable(
    statute_vs_environment_attribution,
    'How much of the cost borne by notice-exposed couples is authored by the statute''s design (the publication requirement) versus imposed by the surrounding communal enforcement environment that the statute merely renders visible?',
    'Counterfactual comparison with confidential-registration variants in comparable legal environments, isolating the marginal effect of publication.',
    'Reassigns epsilon between the arrangement and its environment; a small statutory share would move this reading toward rope even while aggregate harm stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statute_vs_environment_attribution, conceptual, 'Attribution of the exposure harm between statutory design and surrounding social enforcement.').

omega_variable(
    formal_vs_realized_gender_equity,
    'Does the act''s symmetrical divorce and maintenance architecture deliver realized gender equity, or does enforcement lag (multi-year maintenance litigation, evidentiary burdens) leave the formal advantage largely notional?',
    'Longitudinal court-administration data on civil-framework maintenance and divorce outcomes by gender, controlling for income.',
    'If realization lags severely, the beneficiary declaration for women_using_civil_divorce_rights weakens and the arrangement''s coordination credit shrinks accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_realized_gender_equity, empirical, 'Gap between the act''s formal gender symmetry and its realized outcomes.').

omega_variable(
    ucc_template_trajectory,
    'Will the act remain a minority refuge alongside personal law, or become the template for a national uniform civil code replacing the parallel regimes?',
    'Track state-level uniform-code enactments and parliamentary movement; observe whether the act''s machinery (registration, officers, grounds) is carried into any successor code.',
    'As a template, its scope universalizes and enforcement intensity rises, raising both its coordination value and its exposure costs; as a refuge, it persists as a narrow exit channel with stable parameters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ucc_template_trajectory, preference, 'Open policy trajectory question affecting the arrangement''s future scope and intensity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_secular_civil_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t0, observed).
narrative_ontology:measurement(marriage_secular_civil_tr_t10, marriage_authority_kernel__secular_civil_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t10, observed).
narrative_ontology:measurement(marriage_secular_civil_tr_t20, marriage_authority_kernel__secular_civil_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t20, observed).
narrative_ontology:measurement(marriage_secular_civil_tr_t30, marriage_authority_kernel__secular_civil_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t30, observed).
narrative_ontology:measurement(marriage_secular_civil_tr_t40, marriage_authority_kernel__secular_civil_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t40, observed).
narrative_ontology:measurement(marriage_secular_civil_tr_t50, marriage_authority_kernel__secular_civil_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t50, observed).
narrative_ontology:measurement(marriage_secular_civil_tr_t60, marriage_authority_kernel__secular_civil_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t60, observed).
narrative_ontology:measurement(marriage_secular_civil_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.38).
narrative_ontology:measurement_basis(marriage_secular_civil_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(marriage_secular_civil_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t0, observed).
narrative_ontology:measurement(marriage_secular_civil_be_t10, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t10, observed).
narrative_ontology:measurement(marriage_secular_civil_be_t20, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t20, observed).
narrative_ontology:measurement(marriage_secular_civil_be_t30, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t30, observed).
narrative_ontology:measurement(marriage_secular_civil_be_t40, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t40, observed).
narrative_ontology:measurement(marriage_secular_civil_be_t50, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t50, observed).
narrative_ontology:measurement(marriage_secular_civil_be_t60, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t60, observed).
narrative_ontology:measurement(marriage_secular_civil_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.42).
narrative_ontology:measurement_basis(marriage_secular_civil_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(marriage_secular_civil_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t0, observed).
narrative_ontology:measurement(marriage_secular_civil_su_t10, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t10, observed).
narrative_ontology:measurement(marriage_secular_civil_su_t20, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t20, observed).
narrative_ontology:measurement(marriage_secular_civil_su_t30, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t30, observed).
narrative_ontology:measurement(marriage_secular_civil_su_t40, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t40, observed).
narrative_ontology:measurement(marriage_secular_civil_su_t50, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 50, 0.23).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t50, observed).
narrative_ontology:measurement(marriage_secular_civil_su_t60, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 60, 0.24).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t60, observed).
narrative_ontology:measurement(marriage_secular_civil_su_t70, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 70, 0.25).
narrative_ontology:measurement_basis(marriage_secular_civil_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Indian marriage law' decomposes into five structurally distinct readings of the marriage_authority_kernel, one per standing arrangement (Special Marriage Act 1954; Hindu Marriage Act 1955; Shariat application; Indian Christian Marriage Act 1872; Parsi Marriage and Divorce Act 1936). Each reading has its own epsilon, its own beneficiary/victim sets, and its own adjudicating bodies; this file instantiates the secular_civil_reading. The upstream constitutional-individual-rights frame influences the downstream personal-law readings (codification shape, constitutional review) without foreclosing them; family members are linked via affects_constraints for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

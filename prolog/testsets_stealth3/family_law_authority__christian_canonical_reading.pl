% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Marriage as Sacrament under Ecclesiastical Authority — Christian Canonical Reading
 *   domain: legal/political/religious
 *
 * SUMMARY:
 *   Under the christian_canonical_reading of the family_law_authority kernel,
 *   marriage is constituted as a sacrament (Catholic) or a church-governed
 *   covenant (Protestant), and its validity, permanence, and dissolution fall
 *   under ecclesiastical jurisdiction: the church prescribes the required
 *   form, adjudicates whether particular unions were validly contracted, and
 *   disciplines members through communion access. The arrangement solves real
 *   coordination problems — verification of unions, impediment screening,
 *   deterrence of clandestine marriage, credible lifetime care commitments —
 *   while extracting deference, material support, and jurisdictional control
 *   from laity, and imposing severe costs on identifiable classes (spouses in
 *   unions tribunals decline to null, the divorced-and-remarried barred from
 *   communion, petitioners subjected to intrusive scrutiny). Assumptions
 *   stated: the epsilon referent is the standing ecclesiastical-authority
 *   arrangement itself, assessed by this reading's own lights — never the
 *   secular alternative it competes with; the metrics weight the Catholic
 *   juridical core per the kernel's expected structural delta, with the
 *   intra-reading Catholic/Protestant variance carried as an omega rather
 *   than hedged into epsilon; the measurement interval anchors at the Council
 *   of Trent (whose decree on canonical form marks the great enforcement
 *   intensification) and closes at the 2015 tribunal-streamline reform. The
 *   claim (tangled_rope) and the metrics are independently authored facts;
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - magisterium_and_tribunals: Agenda-setter (institutional/arbitrage) — declares doctrine on validity and permanence, writes and revises canon law, runs diocesan tribunals; collects deference, fees, and authority; alone controls the interpretive machinery through which exceptions are processed
 *   - - ordained_parish_clergy: Beneficiary (organized/constrained) — officiates under the required form, prepares couples, refers dissolution questions upward; draws livelihood, status, and pastoral authority from administering the system
 *   - - devout_laity: Coordinated beneficiary with payer costs (organized/identity_locked) — receives the standardized family framework and transmits it; pays tithes, offerings, volunteer labor, and deference; cannot contemplate exit without losing faith community and self-concept
 *   - - trapped_spouses_of_valid_marriages: Primary target (powerless/trapped) — in marriages experienced as irretrievably broken but not declared null; exits are discretionary annulment, spousal death, or apostasy
 *   - - divorced_remarried_denied_communion: Sanctioned target (moderate/identity_locked) — lives under open communion denial while remaining attached, attending, contributing, and absorbing the sanction
 *   - - annulment_petitioners: Dual-positioned target-beneficiary (moderate/trapped) — purchases exit by submitting their past life to institutional judgment; outcomes hinge on tribunal personnel and paperwork
 *   - - civil_family_law_regimes: Excluded rival authority (institutional/arbitrage) — runs parallel registration and dissolution systems this reading does not recognize as touching valid sacramental marriages
 *   - - comparative_family_law_scholars: Analytical observer (analytical/analytical) — studies the jurisdictional partition across eras; no stake in the arrangement's persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.62).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.6).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Marriage as Sacrament under Ecclesiastical Authority — Christian Canonical Reading").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "legal/political/religious").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '0fbd1bdc-613c-4d53-af22-4120287540ef').
narrative_ontology:cs_kernel_codification('0fbd1bdc-613c-4d53-af22-4120287540ef', formalized).
narrative_ontology:cs_authority_grounding('0fbd1bdc-613c-4d53-af22-4120287540ef', lineage).
narrative_ontology:cs_interpretation_layer_present('0fbd1bdc-613c-4d53-af22-4120287540ef').
narrative_ontology:cs_reading_relation('0fbd1bdc-613c-4d53-af22-4120287540ef', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fbd1bdc-613c-4d53-af22-4120287540ef', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fbd1bdc-613c-4d53-af22-4120287540ef', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fbd1bdc-613c-4d53-af22-4120287540ef', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('0fbd1bdc-613c-4d53-af22-4120287540ef', foundational, marriage_indissoluble_by_divine_institution).
narrative_ontology:cs_axiom_status(marriage_indissoluble_by_divine_institution, holdable).
narrative_ontology:cs_axiom_grounding('0fbd1bdc-613c-4d53-af22-4120287540ef', marriage_indissoluble_by_divine_institution, theological).
narrative_ontology:cs_axiom('0fbd1bdc-613c-4d53-af22-4120287540ef', secondary, ecclesial_authority_determines_marital_validity).
narrative_ontology:cs_axiom_status(ecclesial_authority_determines_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('0fbd1bdc-613c-4d53-af22-4120287540ef', ecclesial_authority_determines_marital_validity, conventional).
narrative_ontology:cs_reference_frame('0fbd1bdc-613c-4d53-af22-4120287540ef', indissoluble_sacramental_jurisdiction).
narrative_ontology:cs_drift_state('0fbd1bdc-613c-4d53-af22-4120287540ef', contemporary_post_streamline_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fbd1bdc-613c-4d53-af22-4120287540ef', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, magisterium_and_tribunals).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ordained_parish_clergy).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, devout_laity).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, trapped_spouses_of_valid_marriages).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_remarried_denied_communion).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, annulment_petitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, annulment_petitioners).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, devout_laity).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, canonical_form_necessity_tametsi).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, ecclesial_jurisdiction_over_matrimony).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares doctrine on the nature and permanence of marriage, writes and revises canon law, operates diocesan tribunals that judge whether particular unions were validly contracted, and disciplines members through communion access. Collects tribunal fees, deference, and institutional authority, and sets the terms on which remarriage becomes possible. Its position depends on the core teaching remaining unchanged, while it alone controls the interpretive machinery through which practical exceptions are processed.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, magisterium_and_tribunals, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, magisterium_and_tribunals, beneficiary).

% Officiates weddings under the required form, prepares couples, keeps registers, and refers dissolution questions to tribunals. Draws salary, housing, status, and pastoral authority from administering the marriage system. Vows and ordination make departure costly, and professional identity is bound up with the sacramental system served.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ordained_parish_clergy, beneficiary,
    organized, biographical, constrained, global).

% Receives a standardized framework for forming families — courtship norms, wedding rites, impediment screening, communal recognition, parish support networks — and transmits it to children. Pays tithes, offerings, and extensive volunteer labor sustaining parishes, and owes deference to clerical judgment on family decisions. Leaving would mean leaving the faith community and a self-understood identity; most members do not regard that as a live option even when they chafe at particular rules.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, devout_laity, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, devout_laity, payer).

% Lives in marriages experienced as irretrievably broken but not declared null by tribunals. Available exits inside the system: a successful annulment petition (discretionary, costly, uncertain), the death of a spouse, or leaving the church entirely. Until one occurs, no new union is recognized by the community; forming one anyway costs communion and standing.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, trapped_spouses_of_valid_marriages, payer,
    powerless, biographical, trapped, global).

% Ended a prior marriage by civil divorce and formed a new union, and now lives under open sanction: barred from communion, designated irregular, sometimes removed from ministries and parish roles. Most remain attached believers — attending, raising children in the faith, contributing money and labor — and absorb the sanction rather than leave, because leaving would cost the community and identity the sanction threatens.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_remarried_denied_communion, payer,
    moderate, biographical, identity_locked, global).

% Submits a broken marriage to tribunal scrutiny: assembling documents, testifying under examination about courtship and consent, paying fees that are often subsidized, waiting months or years. A favorable decision restores eligibility to marry in the church; outcomes depend heavily on tribunal staffing, documentation quality, and local practice. The process puts a person's past intimate life under institutional judgment as the price of exit.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, annulment_petitioners, payer,
    moderate, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, annulment_petitioners, beneficiary).

% Runs parallel registration and dissolution systems under state law. In concordat countries its jurisdiction was historically subordinated to canon law; elsewhere it competed with the church system and won the general population. From inside this reading its decrees do not dissolve valid sacramental marriages, and its claim to family-law authority is precisely what the reading's jurisdictional assertion excludes from the conversation.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_family_law_regimes, excluded,
    institutional, generational, arbitrage, national).

% Studies how ecclesiastical and civil systems partition marriage jurisdiction across eras and countries, publishing analyses the church reads unevenly. Holds no stake in the arrangement's persistence and can place it alongside sibling systems without allegiance to any.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, comparative_family_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, magisterium_and_tribunals).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the formation of marriages across dispersed congregations: screens impediments (consanguinity, affinity, prior bonds), prescribes required form (authorized officiant, witnesses, records), registers unions centrally, adjudicates disputed validity and dissolution through tribunals, and surrounds family life with rites, catechesis, and mutual-aid structures. It solves verification and commitment problems that scattered premodern communities could not solve bilaterally: proving a union occurred, deterring clandestine or bigamous ones, and making lifetime care commitments credible.
% TRANSFER_FUNCTION: Moves deference and obedience on intimate-life decisions from laypersons and families to clerical officeholders; moves material support (tithes, offerings, tribunal fees, volunteer labor) from laity to parishes and dioceses; and moves adjudicative jurisdiction over marriage formation and dissolution from civil or familial hands to ecclesiastical courts. Historically it also transmitted legitimacy itself: children's status, inheritance, and community standing flowed through church-recognized unions.
% ABSENT_VOICES: Civil family-law regimes and the couples they register stand outside this reading's adjudication — the rival authority is not seated at the table. Spouses in mixed marriages historically faced dispensations and privilege-dissolutions negotiated largely over their heads. Wives in unbreakable unions of earlier centuries rarely had standing to initiate proceedings; tribunals heard petitions, but the machinery was staffed, priced, and scheduled by the hierarchy. Contemporary organized groups of divorced-and-remarried faithful advocate for change and remain outside the decision channels.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen validity and dissolution questions for every union in the frame: remarriage eligibility flips for the divorced, tribunal adjudications vanish mid-process, parish economies lose marriage-linked revenue and volunteer labor, concordat and mixed-marriage arrangements unwind, and the communion-discipline system loses its principal object. Civil systems would absorb registration and dissolution, but the identity consequences for adherents would rearrange communities rather than merely swapping administrators.
% FOUNDING_PROBLEM: Early and medieval Christianity confronted unions formed without witnesses or records, bigamy, unilateral repudiation of wives, and marriages across religious lines — with weak or absent civil registration to police any of it. The arrangement was built to make unions verifiable, permanent, and church-governed: to stop clandestine marriage, protect wives and children against arbitrary male dissolution as the tradition framed that duty, and bind family formation into the salvation-economy the church administered.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties, and it cuts both ways: secular historiography of canon law (scholarship on clandestine marriage and the Tridentine response) attests that the original verification gap was real; comparative family-law scholarship attests that civil registration and divorce law have since absorbed those regulatory functions in most jurisdictions. The claim that the founding problem is still live rests chiefly on magisterial documents — on the benefiting parties themselves. Stated plainly: external sources corroborate a real founding problem that is now largely closed; only internal sources attest continuing necessity, and that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.62: the arrangement's extraction is real but not total — formation standards, impediment screening, and communal support are delivered as promised, and most participants are net-willing; the concentrated costs fall on the trapped, sanctioned, and petitioning classes, and on the diffuse deference-and-resources flow from the whole laity to the hierarchy. Suppression is 0.60 as a raw structural property (unscaled by power or scope): enforcement is active (tribunals, communion discipline, required canonical form) and alternatives are suppressed within the frame, but in pluralistic societies civil exit exists externally, so the binding force is partly identity-lock rather than bare barrier. Theater is 0.31 and rising: tribunal nullity grants increasingly declare to have never existed marriages that patently functioned as marriages — performative maintenance of an absolutist doctrine — though weddings, adjudication, and parish life remain substantively functional. Accessibility_collapse is 0.60: understanding the sacramental frame closes the civil-only and divorce options internally for adherents, while external civil exits persist. Resistance is 0.60: the Reformation began partly as resistance to this jurisdiction; secularization movements, concordat renegotiations, organized advocacy by the divorced-and-remarried, and internal magisterial dissent have contested it continuously. Temporal series share one eight-point grid (1563-2015) across all three tracked metrics: enforcement peaks at Trent (0.82), decays as civil marriage spreads (0.72 by 1850), partially re-tightens under the 1983 disciplinary restoration (0.66), and relaxes with the 2015 tribunal streamlining (0.60); base extractiveness follows a parallel arc with a 1985 uptick; theater climbs late as the nullity apparatus expands.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the structure hands them different objects. From the magisterial seat the arrangement is a treasury of administered permanence: cases resolved, doctrine guarded, exceptions processed mercifully through the tribunal the seat itself controls. From the trapped-spouse seat the same tribunal is an arbitrary gate whose outcome depends on paperwork and personnel, holding their life hostage to discretion. From the devout-laity seat it is a gift and a discipline simultaneously — received framework, paid deference. From the sanctioned seat it is a wound carried inside belonging: communion denied, community retained. The engine computes these per-seat divergences from power, exit, and directional position; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the magisterium and tribunals subsidize themselves from the arrangement (lowest d — they collect fees, deference, jurisdiction, and volunteer labor, and the gain_flow receipt lands on this seat); parish clergy collect livelihood and status from administering it (low d, dampened further by their constrained exit); devout laity receive the coordination framework and pay only diffuse costs (low-to-symmetric d, with their secondary payer role registering tithes and deference). Victim declarations map to high directionality: trapped spouses sit nearest the full-target end (high d, amplified by trapped exit); the divorced-and-remarried carry high d amplified by identity-lock — they cannot leave, so the extraction binds; annulment petitioners hold high d while in process, partially offset by the benefit realized on success, which their secondary beneficiary role encodes. Civil regimes are excluded rather than coordinated — the reading does not recognize their jurisdiction, so they sit outside the d computation as a rival claimant.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is load-bearing here in both directions. Labeling the arrangement a pure snare would erase why billions participated voluntarily and why its formation standards solved genuine verification and commitment problems that scattered communities could not solve bilaterally; the coordination function is real and continuously exercised. Labeling it a pure rope would erase the tribunal gatekeeping rents, the communion-denial discipline aimed at the remarried, and the identifiable trapped classes whose exit runs through institutional discretion. It is not a scaffold: no sunset clause exists and the doctrine claims perpetual, divinely instituted status. It is not a piton: enforcement is active, adjudication volume is real, and the administrator both maintains the arrangement and profits from it — the cost-asymmetry test for piton fails because a concentrated beneficiary exists. It is not a mountain: nothing about it emerges naturally; every element is enacted and enforced, which is why resistance has moved it repeatedly across the interval. On fixing_cost: the hierarchy can cheaply tune enforcement practice (it did, in 2015) but cannot fix — remove — the constraint itself, because indissolubility is held divinely instituted and irreformable; revision would unravel the authority structure's self-grounding. Hence fixing_cost is prohibitive even though marginal enforcement adjustments are cheap. The founding problem is contested rather than dead: the regulatory gap the arrangement addressed was real and is now largely absorbed by civil law, but the hierarchy attests continuing necessity — the R5 mismatch surface, not the narrative, is what consumers should read.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This classification describes only the christian_canonical_reading of the family_law_authority kernel — do downstream consumers pool it with sibling readings (hindu_dharmashastra, muslim_shariat, parsi_zoroastrian, secular_contractual), which instantiate structurally different constraints?',
    'Compare compiled classifications across the five reading-files as separate constraints; verify that epsilon, victim sets, and types are never averaged or transferred across readings.',
    'Pooling would misattribute the secular reading''s low extraction or the shariat reading''s repudiation asymmetries to this arrangement; keeping readings separate keeps per-seat chi computations indexical to THIS reading''s beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame guard: this story is one reading of a contested kernel, classified in isolation.').

omega_variable(
    denominational_composition_variance,
    'How much does the reading''s internal Catholic/Protestant composition change its measured extraction, given that Catholic indissolubility and Protestant governed-divorce are both inside this single reading?',
    'Parameterize the instantiation by denominational weight: recompute epsilon for a mainline-Protestant-majority polity (divorce governable and remarriage possible inside the frame) versus a Catholic-canonical-form polity.',
    'The authored epsilon of 0.62 weights the Catholic juridical core, per the kernel''s expected structural delta; a Protestant-weighted instantiation would sit nearer 0.45-0.50 with softer suppression, because exit-by-divorce exists inside the frame rather than only outside it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denominational_composition_variance, conceptual, 'Intra-reading variance between Catholic no-divorce permanence and Protestant denominational divorce governance.').

omega_variable(
    annulment_substitution_question,
    'Does tribunal annulment function as prohibited-but-gatekept divorce, making the operative cost a discretionary toll rather than an absolute bar?',
    'Grant-rate and processing-time series around the 2015 streamline motu proprio, plus cross-diocese variation; if grants scale with documentation quality and tribunal staffing rather than with underlying facts about consent, gatekeeping rent dominates the trapping effect.',
    'If substitution dominates, the arrangement traps fewer people absolutely but concentrates extraction in discretionary adjudication — raising the tribunal-facing seats'' weight in chi and lowering the trapping component borne by the resigned majority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_substitution_question, empirical, 'Whether the annulment apparatus is a substitute divorce channel under hierarchical gatekeeping.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression holding sanctioned and trapped members structural (communion denial, parish social cost, residual legal entanglement in concordat systems) or internalized (sacramental conscience rendering exit unthinkable even where civil exits exist)?',
    'Post-exit suppression trajectory of lapsed members: if sanction pressure fades immediately upon leaving the fold while distress persists, internalization dominates; if former members report relief once outside enforcement reach, structural mechanisms dominate.',
    'If internalized, measured suppression understates the suppression members carry with them, and enforcement decay alone would predict faster erosion of the arrangement than will actually occur.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism in an identity-locked population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 1563, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1563, family_law_authority__christian_canonical_reading, theater_ratio, 1563, 0.2).
narrative_ontology:measurement_basis(fami_tr_t1563, observed).
narrative_ontology:measurement(fami_tr_t1650, family_law_authority__christian_canonical_reading, theater_ratio, 1650, 0.22).
narrative_ontology:measurement_basis(fami_tr_t1650, observed).
narrative_ontology:measurement(fami_tr_t1750, family_law_authority__christian_canonical_reading, theater_ratio, 1750, 0.24).
narrative_ontology:measurement_basis(fami_tr_t1750, observed).
narrative_ontology:measurement(fami_tr_t1850, family_law_authority__christian_canonical_reading, theater_ratio, 1850, 0.28).
narrative_ontology:measurement_basis(fami_tr_t1850, observed).
narrative_ontology:measurement(fami_tr_t1918, family_law_authority__christian_canonical_reading, theater_ratio, 1918, 0.26).
narrative_ontology:measurement_basis(fami_tr_t1918, observed).
narrative_ontology:measurement(fami_tr_t1965, family_law_authority__christian_canonical_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement_basis(fami_tr_t1965, observed).
narrative_ontology:measurement(fami_tr_t1985, family_law_authority__christian_canonical_reading, theater_ratio, 1985, 0.27).
narrative_ontology:measurement_basis(fami_tr_t1985, observed).
narrative_ontology:measurement(fami_tr_t2015, family_law_authority__christian_canonical_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement_basis(fami_tr_t2015, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t1563, family_law_authority__christian_canonical_reading, base_extractiveness, 1563, 0.72).
narrative_ontology:measurement_basis(fami_be_t1563, observed).
narrative_ontology:measurement(fami_be_t1650, family_law_authority__christian_canonical_reading, base_extractiveness, 1650, 0.74).
narrative_ontology:measurement_basis(fami_be_t1650, observed).
narrative_ontology:measurement(fami_be_t1750, family_law_authority__christian_canonical_reading, base_extractiveness, 1750, 0.75).
narrative_ontology:measurement_basis(fami_be_t1750, observed).
narrative_ontology:measurement(fami_be_t1850, family_law_authority__christian_canonical_reading, base_extractiveness, 1850, 0.68).
narrative_ontology:measurement_basis(fami_be_t1850, observed).
narrative_ontology:measurement(fami_be_t1918, family_law_authority__christian_canonical_reading, base_extractiveness, 1918, 0.66).
narrative_ontology:measurement_basis(fami_be_t1918, observed).
narrative_ontology:measurement(fami_be_t1965, family_law_authority__christian_canonical_reading, base_extractiveness, 1965, 0.64).
narrative_ontology:measurement_basis(fami_be_t1965, observed).
narrative_ontology:measurement(fami_be_t1985, family_law_authority__christian_canonical_reading, base_extractiveness, 1985, 0.66).
narrative_ontology:measurement_basis(fami_be_t1985, observed).
narrative_ontology:measurement(fami_be_t2015, family_law_authority__christian_canonical_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement_basis(fami_be_t2015, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1563, family_law_authority__christian_canonical_reading, suppression_requirement, 1563, 0.82).
narrative_ontology:measurement_basis(fami_su_t1563, observed).
narrative_ontology:measurement(fami_su_t1650, family_law_authority__christian_canonical_reading, suppression_requirement, 1650, 0.79).
narrative_ontology:measurement_basis(fami_su_t1650, observed).
narrative_ontology:measurement(fami_su_t1750, family_law_authority__christian_canonical_reading, suppression_requirement, 1750, 0.77).
narrative_ontology:measurement_basis(fami_su_t1750, observed).
narrative_ontology:measurement(fami_su_t1850, family_law_authority__christian_canonical_reading, suppression_requirement, 1850, 0.72).
narrative_ontology:measurement_basis(fami_su_t1850, observed).
narrative_ontology:measurement(fami_su_t1918, family_law_authority__christian_canonical_reading, suppression_requirement, 1918, 0.69).
narrative_ontology:measurement_basis(fami_su_t1918, observed).
narrative_ontology:measurement(fami_su_t1965, family_law_authority__christian_canonical_reading, suppression_requirement, 1965, 0.63).
narrative_ontology:measurement_basis(fami_su_t1965, observed).
narrative_ontology:measurement(fami_su_t1985, family_law_authority__christian_canonical_reading, suppression_requirement, 1985, 0.66).
narrative_ontology:measurement_basis(fami_su_t1985, observed).
narrative_ontology:measurement(fami_su_t2015, family_law_authority__christian_canonical_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement_basis(fami_su_t2015, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, attachment_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'religious governance of marriage' covers five structurally distinct arrangements; per the epsilon-invariance principle each is a separate constraint story with its own epsilon, victim set, and type, linked as a constraint family. This file authors the christian_canonical_reading alone: its extraction is driven by tribunal-gatekept sacramental permanence and communion discipline. Expected deltas for siblings: the secular_contractual_reading's extraction is driven by registration and dissolution market power rather than sacramental trapping; the muslim_shariat_reading's victim structure centers on repudiation asymmetries inside a contract form; the hindu_dharmashastra and parsi_zoroastrian readings center on customary and community-enforced obligation. The edges declare the family so contamination propagation and cross-reading comparison treat the five as kin, never as one measurable thing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling as Voluntary Dispute-Resolution Protocol (Institutional Displacement Reading)
 *   domain: legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint story instantiates the INSTITUTIONAL DISPLACEMENT READING
 *   of the dueling-disappearance kernel. The reading asserts that dueling
 *   declined primarily because courts, banking reputation mechanisms, and
 *   libel law emerged as functionally superior dispute-resolution systems
 *   that outcompeted dueling on transaction cost and enforceability. Dueling
 *   was never formally mandatory; as institutional alternatives became
 *   available and accessible, practitioners voluntarily substituted them. The
 *   constraint remains a ROPE (voluntary coordination on a protocol)
 *   throughout: it is not a snare (no victims coerced to participate), not a
 *   mountain (it is a human institution), and not a piton (the function was
 *   real; it simply became obsolete as better alternatives emerged). The
 *   claimed type does not change across the interval—what changes is the
 *   practitioners' participation, which declines as institutional
 *   alternatives improve. This reading coexists with sibling readings that
 *   emphasize cultural displacement (contraction_reading) and causal
 *   overdetermination (overdetermined_composite_reading); it does not
 *   foreclose them. The kernel itself is the contestable fact: what DID cause
 *   dueling to disappear? This reading proposes institutional competition;
 *   siblings propose cultural axiom shift or multiple independent causes.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: maintain dueling as coordination mechanism; shift to alternatives as transaction costs favor them
 *   - common_law_courts: develop libel and defamation remedies; displace dueling by offering legal recourse
 *   - banking_and_credit_networks: create reputation mechanisms; outcompete honor-code solutions
 *   - state_legislatures: criminalize dueling (1800s onward); formalize institutional displacement by law
 *   - newspapers_and_print_media: both benefit from dueling (sensational coverage) and displace it (mass libel liability law)
 *   - middle_class_professionals: gain from institutional alternatives; shift away from honor-culture protocols
 *   - excluded_lower_classes: locked out of both dueling and legal/banking alternatives; absent from the substitution choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.22).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling as Voluntary Dispute-Resolution Protocol (Institutional Displacement Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, 'e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd').
narrative_ontology:cs_kernel_codification('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', distributed).
narrative_ontology:cs_authority_grounding('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', distributed).
narrative_ontology:cs_reading_relation('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', foundational, institutional_competition_outcompetes_custom).
narrative_ontology:cs_axiom_status(institutional_competition_outcompetes_custom, holdable).
narrative_ontology:cs_axiom_grounding('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', institutional_competition_outcompetes_custom, empirically_contingent).
narrative_ontology:cs_axiom('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', secondary, transaction_cost_drives_mechanism_selection).
narrative_ontology:cs_axiom_status(transaction_cost_drives_mechanism_selection, holdable).
narrative_ontology:cs_axiom_grounding('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', transaction_cost_drives_mechanism_selection, instrumental).
narrative_ontology:cs_reference_frame('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', honor_culture_coordination_apex).
narrative_ontology:cs_drift_state('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', institutional_alternatives_ascendant, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6b0f27f-f3fd-488c-b8ee-5463c9c75ffd', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, newspapers_and_print_media).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, middle_class_professionals).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, institutional_competition_displaces_customs).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, coordination_mechanisms_rival_on_transaction_cost).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen who initiate and participate in dueling as a dispute-resolution mechanism for matters of reputation and honor. They benefit from the coordination protocol itself—it solves a problem they recognize (restoring honor when informal means fail). Early in the interval (1770–1820), they have few alternatives; late in the interval (1880–1920), they have institutional alternatives available but choose dueling for cultural/identity reasons or out of stubborn adherence to honor norms. Their exit options shift from 'trapped' to 'mobile' as courts and banking systems mature.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_practitioners, beneficiary,
    powerful, biographical, mobile, national).

% Develop and enforce legal remedies for defamation, fraud, slander, and libel that capture disputes previously handled by dueling. They set pleading standards, evidence rules, damages caps, and exclude violence as a settlement mechanism. They do not mandate use of courts in place of dueling—they simply make courts available as a superior alternative for settling reputation disputes. Courts gain political power and cultural legitimacy over the interval as their remedies prove faster and more enforceable than dueling.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, common_law_courts, agenda_setter,
    institutional, generational, analytical, national).

% Create and maintain reputation mechanisms (credit ratings, merchant networks, letters of credit, insider information networks) that allow participants to establish trustworthiness and assess risk without resorting to honor-based violence. These systems grow in scope and importance over the interval, especially after 1820. They outcompete dueling by making reputation quantifiable, tradeable, and enforceable through economic incentives rather than combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_and_credit_networks, agenda_setter,
    institutional, generational, analytical, regional).

% Incrementally criminalize dueling (first as murder, later as specific statutory offenses) throughout the 1800s. The criminalization is a response to institutional competition already underway—legislatures formalize institutional displacement by law, accelerating the decline that market forces (transaction costs) have already driven. States vary in the timing and severity of criminalization, suggesting that legal prohibition is secondary to institutional displacement rather than primary.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Early in the interval (1770–1850), newspapers benefit from sensational dueling coverage—duels are newsworthy events. Over time, as mass printing becomes profitable and libel law develops, newspapers shift toward institutional solutions: they create reputation mechanisms separate from honor dueling (a newspaper's credibility becomes an alternative to a gentleman's reputation). They have a mixed role: partly dependent on dueling for coverage, partly interested in displacing it by creating alternative reputation mechanisms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, newspapers_and_print_media, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, newspapers_and_print_media, agenda_setter).

% Lawyers, merchants, doctors, and businessmen whose livelihoods depend on institutional alternatives to dueling—courts, banks, professional networks, licenses. As this group grows in political power over the interval, they push for institutional strengthening and dueling prohibition. They are not victims of dueling; rather, they benefit from institutional alternatives and have little stake in honor-culture protocols.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, middle_class_professionals, beneficiary,
    organized, biographical, mobile, national).

% Formally excluded from dueling by class status (only gentlemen could duel; commoners who attempted dueling were prosecuted, whipped, or socially shamed). They are also largely excluded from access to courts and banking reputation mechanisms. They are absent from both the dueling coordination and the institutional alternatives—locked out of the dispute-resolution choices available to the powerful.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, excluded_lower_classes, excluded,
    powerless, biographical, trapped, national).

% Examines the historical record from outside the constraint to understand the mechanism of institutional displacement. Observes that dueling was a coordination protocol solving a real problem; as alternatives emerged on the basis of lower transaction costs and greater enforceability, practitioners voluntarily switched. No coercion was necessary; institutional competition displaced dueling through superior performance.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__institutional_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__institutional_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dispute-resolution problem for matters of personal honor and reputation when informal reconciliation fails and legal remedies are unavailable or inadequate. The dueling protocol coordinates gentlemen on a shared understanding: a challenge can be issued for insult or defamation, the defendant can accept or refuse, witnesses and seconds structure the encounter, combat occurs under agreed rules, and satisfaction is achieved through the defendant's concession or death. This mechanism allows a gentleman to restore his reputation when his honor has been damaged.
% TRANSFER_FUNCTION: Transfers risk and mortality: the challenger transfers the risk of death to the defendant in exchange for a retraction (satisfaction) or the defendant's death/defeat (restoration of honor through recognized victory). The seconds and witnesses transfer coordination effort to facilitate the exchange and verify the conduct under the protocol.
% ABSENT_VOICES: Lower classes are structurally excluded from dueling by class norms and legal penalties—they cannot participate in the coordination as gentlemen. Reformers, religious authorities, and anti-dueling advocates who oppose honor-culture norms entirely are absent from the coordination—they would argue for alternative dispute-resolution mechanisms and against the honor-culture axioms that justify dueling, but they are not seated as parties to the dueling protocol. Legal authorities and banking institutions (eventually) become seated as alternatives, but in the early period (1770–1820) they are excluded from the honor-culture conversation.
% DISAPPEARANCE_RATIONALE: If dueling disappeared overnight (which it essentially did through the 19th century), the world rearranges around institutional alternatives: reputation disputes move to courts and libel law, financial disputes move to banking and credit networks, public disputes move to newspapers and mass media. The rearrangement is VOLUNTARY SUBSTITUTION, not coerced displacement: practitioners switch to alternatives because they offer superior transaction costs, enforceability, and risk profiles. The world does not stay roughly unchanged; the institutional alternatives become the primary dispute-resolution mechanism.
% FOUNDING_PROBLEM: In honor-culture societies, certain disputes about reputation, insult, and personal honor cannot be fully resolved by informal reconciliation or by common law courts (which do not recognize many honor-based harms as actionable damages). A gentleman's reputation is his primary asset in social and economic contexts; an unresolved insult leaves that asset damaged and vulnerable to further damage. Dueling emerged as a coordination mechanism to solve this problem: it provides a clear, widely recognized protocol for settling such disputes and restoring honor through recognized ritual combat.
% FOUNDING_PROBLEM_CORROBORATION: Historians of honor culture (Bertram Wyatt-Brown, Stephen Ayers, Joanne Freeman) document from primary sources (diaries, letters, legal records, newspapers) that honor-based disputes were indeed treated as serious and unresolved by available institutions in the 18th and early 19th centuries. Institutional historians (Lawrence M. Friedman on law, Naomi Lamoreaux on banking) document the emergence of legal and financial alternatives that captured these disputes. However, some scholars argue the founding problem was partly constructed post-hoc to justify a practice that served other functions (status display, male bonding, political power signaling). The corroboration is therefore mixed: the problem existed, but its magnitude and necessity are disputed among historians outside the honor-culture community.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.18 at interval end) because dueling involves voluntary participation—no party extracts rents from another by coercion; honor-culture practitioners choose to participate because it solves a coordination problem they recognize. Suppression is also LOW (0.22) because the constraint does not require active enforcement at the institutional level; the decline in dueling is driven by voluntary substitution, not suppression of the practice. Theater is very LOW (0.08) because the function is real and central to the participants: settling honor disputes is not performative—it is the core reason the practice exists. Accessibility collapse is HIGH (0.72) because once courts and banking systems become available, the alternatives to dueling are not mere options—they are clearly superior on transaction cost, risk, and legal enforceability; dueling becomes inaccessible (impossible) as a practical choice for those with access to courts. Resistance is MODERATE (0.35) because some honor-culture practitioners resist the institutional displacement (they continue dueling, challenge the legitimacy of legal remedies for honor, view courts as inadequate for certain disputes) even as the practice declines. The measurement series shows a gradual increase in extractiveness and suppression as institutional alternatives accumulate (1770–1880), then stabilization (1880–1920) as dueling becomes truly fringe—a final group of holdouts who duel for cultural attachment, not institutional necessity. The theater ratio remains very low because the function never becomes purely performative; practitioners who duel in 1900 still believe it solves a problem (even if most have switched to alternatives).
 *
 * PERSPECTIVAL GAP:
 *   From the honor-culture seat (practitioners), dueling is a legitimate coordination mechanism that solves a real problem. From the institutional seat (courts, banks, legislatures), dueling is an inefficient anachronism being outcompeted by superior systems. Neither perspective is wrong—they are reading the same constraint from different structural positions. The engine computes the honor-culture seat's directionality as near 0.0 (beneficiary/low-d) because they initiate and benefit from the coordination; the institutional seat's directionality is analytical (outside the protocol). There is no seat divergence in TYPE because the institutional seats are not governed by the constraint—they are competing alternatives. The gap is a FUNCTIONAL gap: dueling solves the dispute-resolution problem for honor disputes; courts + banking solve it better; therefore the constraint declines.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has no victims because it frames dueling as a voluntary protocol with low extraction. Practitioners choose to use it or not; as alternatives emerge, they switch. The beneficiaries are honor-culture practitioners who want a mechanism to settle reputation disputes—they benefit from the coordination protocol itself, not from coercion of others. The directionality of honor_culture_practitioners is near 0.0 (beneficiary, low d) because they are the ones who benefit from the protocol. The directionality of institutional actors (courts, banks, legislatures) is analytical—they are seats that observe the competition but do not participate in the dueling protocol itself. The critical asymmetry is TEMPORAL: early in the interval (1770), honor_culture_practitioners are trapped between dueling and ineffective alternatives; they benefit from dueling and have high stakes in it. Late in the interval (1920), they have mobile exit options—they CAN switch to courts, and most do, leaving only cultural holdouts. The constraint's directionality remains rope-like throughout because the asymmetry is institutional, not exploitative: practitioners voluntarily switch as transaction costs favor them.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (a constraint's mandate has outlived its function) is PARTIALLY APPLICABLE here, but in reverse: the founding problem (unresolved honor disputes) is CONTESTED whether it is dead or live. If we accept the institutional-displacement reading, the founding problem is substantially solved by institutional alternatives by 1880; if we accept the cultural-displacement reading, the founding problem was partly illusory (honor concerns were never as primary as practitioners claimed). The censoring fact is that dueling's decline is VOLUNTARY and GRADUAL, not sudden and coerced—this is inconsistent with mandatrophy, which typically implies a constraint persisting against the will of those it governs. Instead, this reading describes a constraint OUTCOMPETED: it persists as long as its function is unmet, declines as institutional alternatives meet the function better. The final group of holdouts who duel in 1900 are making a deliberate choice to use an inferior mechanism for cultural/identity reasons, not because they are mandated to or coerced into dueling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coerced_substitution,
    'Did practitioners abandon dueling voluntarily because institutional alternatives were superior, or were they coerced by legal prohibition and social stigma into substituting institutional mechanisms they would not have chosen otherwise?',
    'Primary-source analysis of practitioners'' private writings (diaries, letters) during the period 1800–1880 to establish whether they frame switching to courts/banking as a rational choice or as forced compliance. Comparison with jurisdictions where dueling was tolerated longer to see if the decline was identical (suggesting cultural axiom shift) or delayed (suggesting legal coercion matters).',
    'If coerced, the constraint may be better characterized as a SNARE (institutional actors enforcing prohibition despite practitioner preference) than a ROPE (voluntary coordination). If voluntary, this reading holds: institutional competition displaced dueling without coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_substitution, empirical, 'Whether the institutional displacement was voluntary or coerced.').

omega_variable(
    institutional_competition_vs_cultural_axiom_shift,
    'Did practitioners switch to institutional alternatives because courts and banking offered superior transaction costs and enforceability, or did they switch because the honor-culture axiom (reputation as primary value) was displaced by a dignity-culture axiom (intrinsic worth independent of reputation)?',
    'Analyze the timeline and geography of change: if institutional-competition driving, regions with better courts and banking should see faster decline (institutional factor); if cultural-axiom shift driving, the decline should be synchronous across regions with widely varying institutional development (cultural factor). Examine practitioner rhetoric: institutional-competition framing emphasizes efficiency and enforceability; cultural-axiom framing emphasizes changing values and identity.',
    'If cultural-axiom shift is primary, this reading (institutional displacement) is incomplete and the contraction_reading is primary. If institutional competition is primary, this reading correctly identifies the mechanism and the contraction_reading is secondary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_competition_vs_cultural_axiom_shift, conceptual, 'Whether institutional competition or cultural axiom shift drove the decline.').

omega_variable(
    banking_reputation_mechanism_functional_scope,
    'Do banking reputation mechanisms (credit ratings, merchant networks, letters of credit) actually solve the same problem dueling solved (restoration of honor/reputation after insult), or do they solve a different problem (risk assessment for financial transactions)?',
    'Examine primary sources from banking history and credit networks: do they explicitly claim to restore honor or reputation, or only to manage financial risk? Interview histories of reputation and credit. If the functions differ, banking does not displace dueling but creates a parallel alternative for a narrower problem.',
    'If banking solves a different problem, dueling''s decline cannot be fully explained by institutional displacement; cultural shift becomes more plausible as the primary driver. If banking does solve the honor/reputation problem (by making reputation quantifiable and tradeable), this reading gains strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(banking_reputation_mechanism_functional_scope, empirical, 'Whether banking reputation mechanisms displace or complement the honor-restoration function of dueling.').

omega_variable(
    kernel_reading_committer_context,
    'Is the institutional-displacement reading defensible as a separate constraint from the contraction_reading, or do they necessarily entail each other (such that adopting one axiom set forecloses the other)?',
    'Logical analysis of the axiom sets: institutional-competition reading assumes honor-culture axioms remain live but institutional alternatives outcompete dueling; contraction-reading assumes honor-culture axioms are replaced by dignity-culture axioms, making dueling unthinkable. Can both axiom sets be held in a single historical framework? If yes, they coexist; if no, they foreclose.',
    'If they coexist, the kernel has multiple live readings and the corpus should carry all of them. If they foreclose (a reading''s core axiom directly contradicts a sibling''s), the reading_relations should be updated to ''forecloses'' rather than ''coexists_with.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_context, conceptual, 'The logical relationship between this reading''s axioms and sibling readings'' axioms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1770, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1770, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1770, 0.02).
narrative_ontology:measurement_basis(duel_tr_t1770, observed).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1820, 0.04).
narrative_ontology:measurement_basis(duel_tr_t1820, observed).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1850, 0.06).
narrative_ontology:measurement_basis(duel_tr_t1850, observed).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1880, observed).
narrative_ontology:measurement(duel_tr_t1920, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t1770, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1770, 0.08).
narrative_ontology:measurement_basis(duel_be_t1770, observed).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1820, 0.14).
narrative_ontology:measurement_basis(duel_be_t1820, observed).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.18).
narrative_ontology:measurement_basis(duel_be_t1850, observed).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1880, 0.19).
narrative_ontology:measurement_basis(duel_be_t1880, observed).
narrative_ontology:measurement(duel_be_t1920, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1920, 0.18).
narrative_ontology:measurement_basis(duel_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1770, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1770, 0.06).
narrative_ontology:measurement_basis(duel_su_t1770, observed).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1820, 0.12).
narrative_ontology:measurement_basis(duel_su_t1820, observed).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement_basis(duel_su_t1850, observed).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1880, 0.24).
narrative_ontology:measurement_basis(duel_su_t1880, observed).
narrative_ontology:measurement(duel_su_t1920, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1920, 0.22).
narrative_ontology:measurement_basis(duel_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__institutional_displacement_reading, 0.08).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is the INSTITUTIONAL DISPLACEMENT READING of a contested kernel. The kernel 'dueling_disappearance_mechanism' decomposes into three structurally distinct constraints, each answering the question 'why did dueling disappear?' with a different causal mechanism. All three readings share the referent (the historical disappearance of dueling) and the time interval (1770–1920), but differ in their ε assessments (institutional-displacement reading: low extraction, voluntary coordination; contraction-reading: extraction from honor-culture practitioners coerced by axiom displacement; composite-reading: overdetermined). The three constraints are linked in network.affects_constraints; each carries its own kernel_context and cs_structure fields documenting the reading's axioms and relations to siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

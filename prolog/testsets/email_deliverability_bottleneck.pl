% ============================================================================
% CONSTRAINT STORY: email_deliverability_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_email_deliverability_bottleneck, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: email_deliverability_bottleneck
 *   human_readable: Email Deliverability Bottleneck in Digital Communication Infrastructure
 *   domain: digital_infrastructure/communication_systems
 *
 * SUMMARY:
 *   Email deliverability has become a structural bottleneck in digital
 *   communication, where legitimate senders face systematic filtering and
 *   throttling by centralized mailbox providers (Gmail, Outlook, Yahoo Mail).
 *   While spam filtering solves a genuine coordination problem (protecting
 *   users from malicious content), the implementation concentrates control
 *   over sender access in the hands of a small number of institutional actors
 *   with strong incentives to extract value from senders. The constraint
 *   exhibits the full spectrum of DR classifications depending on observer
 *   position: small senders experience a snare (trapped, no exit),
 *   medium-market actors experience tangled rope (mixed coordination and
 *   extraction), major providers experience rope (coordination with side
 *   benefit of control), organized standards bodies experience scaffold
 *   (building sunset alternatives), legacy filtering experiences piton
 *   (performative theater), and civilizational analysis risks falsely
 *   naturalizing contingent design as immutable law. The theater_ratio (0.68)
 *   reflects that email filtering compliance (sender authentication, list
 *   hygiene, content rules) has become substantially performative: senders
 *   follow published guidelines yet fail to reach inboxes, and filtering
 *   algorithms remain opaque despite published standards.
 *
 * KEY AGENTS:
 *   - Major Mailbox Providers (Gmail, Outlook, Yahoo): Institutional beneficiaries (institutional/arbitrage) — control sender access, extract value through reputation gatekeeping, benefit from network effects
 *   - Legitimate Small Senders (nonprofits, small business, individuals): Primary victims (powerless/trapped) — no reputation capital, no technical infrastructure, no leverage; cannot exit email as communication channel
 *   - Mid-Market Senders (established organizations): Secondary victims (moderate/constrained) — can invest in infrastructure but at significant cost; face constrained exit options
 *   - Email Standards Bodies (IETF DMARC working group, M3AAWG): Organized coalition (organized/constrained) — building alternative authentication and reputation pathways; maintain agency through standards development
 *   - Spam Filtering Vendors (Barracuda, Proofpoint, etc.): Secondary beneficiaries (institutional/arbitrage) — profit from opacity; maintain gatekeeper role through proprietary reputation systems
 *   - Email Accessibility (epistemic reliability of email as communication medium): Victim collective (powerless/trapped) — inability to deliver legitimate email undermines email's function as reliable communication channel
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing centralized gatekeeping as inherent to email rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(email_deliverability_bottleneck, 0.52).
domain_priors:suppression_score(email_deliverability_bottleneck, 0.65).
domain_priors:theater_ratio(email_deliverability_bottleneck, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(email_deliverability_bottleneck, extractiveness, 0.52).
narrative_ontology:constraint_metric(email_deliverability_bottleneck, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(email_deliverability_bottleneck, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(email_deliverability_bottleneck, tangled_rope).
narrative_ontology:human_readable(email_deliverability_bottleneck, "Email Deliverability Bottleneck in Digital Communication Infrastructure").
narrative_ontology:topic_domain(email_deliverability_bottleneck, "digital_infrastructure/communication_systems").

domain_priors:requires_active_enforcement(email_deliverability_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(email_deliverability_bottleneck, major_mailbox_providers).
narrative_ontology:constraint_beneficiary(email_deliverability_bottleneck, spam_filtering_vendors).
narrative_ontology:constraint_victim(email_deliverability_bottleneck, legitimate_senders).
narrative_ontology:constraint_victim(email_deliverability_bottleneck, small_business_operators).
narrative_ontology:constraint_victim(email_deliverability_bottleneck, nonprofit_communicators).
narrative_ontology:constraint_victim(email_deliverability_bottleneck, email_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGITIMATE SMALL SENDER (SNARE) — Individual businesses, nonprofits, and community organizations cannot escape the deliverability trap. They lack sender reputation, IP infrastructure, and institutional leverage to negotiate with mailbox providers. Emails are filtered or throttled despite compliance with technical standards. No viable exit: switching email providers does not solve the reputation problem, and building sender reputation requires months of volume at costs they cannot afford. Bears maximum extraction.
constraint_indexing:constraint_classification(email_deliverability_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-MARKET SENDER (TANGLED ROPE) — Medium-sized organizations benefit from email coordination (reaching customers, distributing content) while facing moderate extraction. They can invest in sender authentication (SPF, DKIM, DMARC) and reputation management but at significant cost. Exit is constrained by switching costs and the need to maintain email as a communication channel. Mixed experience: genuine coordination function alongside asymmetric extraction through reputation gatekeeping.
constraint_indexing:constraint_classification(email_deliverability_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR MAILBOX PROVIDER (ROPE) — Gmail, Outlook, Yahoo mail coordinate email delivery and filtering through reputation systems and content inspection. These actors benefit from the bottleneck (control over sender access, data on email patterns, leverage over email marketing). But they also genuinely solve a coordination problem: filtering spam and phishing protects users. Net beneficiary with high agency and arbitrage options (can switch allegiances, adjust algorithms, broker partnerships). Experiences the constraint as coordination.
constraint_indexing:constraint_classification(email_deliverability_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMAIL STANDARDS COALITION (SCAFFOLD) — Organized bodies (IETF DMARC working group, M3AAWG, email authentication advocates) are building alternative verification pathways (authenticated domain claims, reputation infrastructure, DKIM/SPF hardening). These pathways aim to reduce the gatekeeper power of mailbox providers by distributing reputation signals. Sunset logic: as DMARC adoption matures and decentralized reputation systems develop, the bottleneck should weaken. Constrained because coordination requires ongoing standards maintenance, but organized enough to maintain agency.
constraint_indexing:constraint_classification(email_deliverability_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY EMAIL FILTERING RITUAL (PITON) — Traditional email filtering rules (content scanning, domain whitelisting, sender reputation scoring) are substantially performative. Algorithms have become black-box theater: senders comply with published guidelines (authentication, list management, content patterns) yet fail to reach inboxes. The ritual persists through institutional inertia and lack of transparent alternatives, not because it effectively separates legitimate from spam. Theater ratio is high because compliance with stated rules does not predict delivery outcomes.
constraint_indexing:constraint_classification(email_deliverability_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some filtering bottleneck is inherent to email as a system: open SMTP architecture creates an incentive structure where volume-based spamming is cheaper than legitimate sending. The tragedy of the commons in email means filtering lag is structural and inevitable. However, this perspective risks naturalizing contingent institutional design (centralized mailbox provider gatekeeping) as immutable law. The engine's false summit detector will flag this as misplaced naturalization.
constraint_indexing:constraint_classification(email_deliverability_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(email_deliverability_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(email_deliverability_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(email_deliverability_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(email_deliverability_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(email_deliverability_bottleneck, TR),
    TR >= 0.70.

:- end_tests(email_deliverability_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Mailbox providers extract value through sender reputation gatekeeping, forced infrastructure investment (authentication, list management), and opaque algorithmic filtering that creates switching costs and compliance uncertainty. The extraction is not as severe as pure gatekeeper monopoly (0.75+) because legitimate senders can eventually build reputation through persistence and some alternative channels (authentication, reputation services) exist. But the extraction is significant because small senders lack the resources to build sender reputation and lack viable alternatives. Suppression (0.65): High. Significant structural barriers include: (1) technical requirements (SPF, DKIM, DMARC setup) that exceed small sender technical capacity, (2) reputation lag (new senders must build volume gradually), (3) blacklist inclusion creating near-total communication failure, (4) lack of transparency (filtering rules are not published; senders cannot diagnose delivery failures), and (5) no meaningful appeal process (getting delisted is informal and slow). Suppression is not absolute (some workarounds exist; medium-market actors can solve it through investment) but is very high for resource-constrained senders. Theater ratio (0.68): High and increasing. Compliance with published email standards (authentication, list management, content rules) does not reliably predict inbox delivery. Senders follow guidelines yet fail to reach inboxes; mailbox providers publish best practices that do not guarantee compliance. The ritual of email filtering has become substantially performative — the theater increased over the interval (2015-2025) as algorithmic opacity increased and senders were forced into ever-more-complex compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival disagreement. Mailbox providers see rope — they coordinate email delivery while legitimately filtering spam. Small senders see snare — they are trapped without agency or exit, bearing full cost of deliverability infrastructure investment. Standards bodies see scaffold — DMARC, DKIM, and alternative reputation systems are building toward reduced provider gatekeeper power. Spam filtering technology (considered as an institutional actor) sees piton — the ritual persists through inertia (senders comply with rules, filters check compliance) even though the predictive power of rule-compliance for spam detection is low. The civilizational analytical observer risks seeing mountain (email filtering lag is inherent to open SMTP) but the structural data reveals contingent institutional design: the bottleneck strengthens and weakens with mailbox provider policy choices, not with immutable technical constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from the agent's structural position relative to the bottleneck. Mailbox providers (beneficiaries with arbitrage options) have d ≈ 0.10 (low extraction experienced), deriving low chi and rope classification. Small senders (victims with trapped exit) have d ≈ 0.92 (high extraction experienced), deriving high chi and snare classification. Mid-market senders (victims with constrained exit) have d ≈ 0.68 (moderate-high extraction experienced), deriving moderate chi and tangled rope classification. Standards coalition (organized with constrained exit building alternatives) have d ≈ 0.55 (moderate extraction with exit path visible), deriving moderate chi and scaffold classification. The analytical observer (analytical position at civilizational scope) risks d ≈ 0.70 (high extraction from a bird's-eye view) that naturalizes contingent institutional choices as law, producing false summit classification.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy by recognizing that the bottleneck is a genuine hybrid of two structurally distinct mechanisms: (1) a coordination function (spam filtering protects users), and (2) an extraction mechanism (mailbox providers use filtering opacity to maintain sender gatekeeper control). The tangled rope classification holds both simultaneously. The snare classification from the small sender perspective is not a contradiction but a perspectival truth — from their position, the extraction dominates and coordination benefit is invisible. The scaffold perspective identifies a real structural pathway: DMARC and decentralized reputation systems are weakening provider gatekeeper power. The piton perspective correctly identifies that much email filtering compliance is performative theater. The false summit (mountain) reveals the analytical risk: treating institutional design as law. The mandatrophy is resolved by accepting that all perspectives are structurally correct — the presheaf over observer positions IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentication_adoption_threshold,
    'At what DMARC/DKIM/SPF adoption rate does the bottleneck cease to function as an extraction mechanism?',
    'Empirical measurement of deliverability rates for authenticated vs unauthenticated senders; correlation with adoption rates over time; regression analysis of authentication adoption and filtering severity',
    'If threshold < 40% adoption: current bottleneck persists despite authentication scaling. If threshold > 70% adoption: narrow path to sunset visible within 5-10 years as standards mature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentication_adoption_threshold, empirical, 'Authentication adoption threshold for bottleneck weakening').

omega_variable(
    mailbox_provider_algorithm_opacity,
    'Does the opacity of mailbox provider filtering algorithms reflect genuine technical necessity or institutional capture of sender access control?',
    'Comparison of transparent filtering systems (user-configurable rules, published algorithms) vs black-box systems; measurement of false positive rates in each; analysis of whether transparency reduces gatekeeper rent extraction',
    'If technical necessity: filtering opacity is unavoidable; scaffold perspective is aspirational. If capture: transparency mechanisms could dramatically reduce bottleneck extractiveness; scaffold perspective identifies real sunsets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mailbox_provider_algorithm_opacity, empirical, 'Whether algorithm opacity is technical necessity or institutional capture').

omega_variable(
    decentralized_reputation_sufficiency,
    'Can decentralized reputation systems (DNSWL, reputation APIs, blockchain-based sender scores) provide spam filtering effectiveness comparable to centralized mailbox provider control?',
    'Technical benchmarking of decentralized reputation systems against centralized filtering on spam detection rates, false positive rates, and resistance to gaming; pilot deployments with alternative mail infrastructure',
    'If sufficient: decentralized alternatives are architecturally viable; mountain perspective is false summit. If insufficient: centralized gatekeeper control remains structurally necessary; constraint reclassifies toward mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_reputation_sufficiency, empirical, 'Whether decentralized reputation can replace centralized filtering').

omega_variable(
    legitimate_sender_coalition_formation,
    'Can legitimate senders (nonprofits, small business, community organizations) organize collectively to demand deliverability standards or negotiate with mailbox providers?',
    'Historical analysis of coalition-building efforts (Email Sender & Provider Coalition, Messaging Malware Mobile Anti-Abuse Working Group); measurement of collective bargaining power; tracking of policy changes driven by organized legitimate senders',
    'If coalition feasible: powerless agents become organized; snare perspective shifts toward tangled rope. If impossible: powerless fragmentation persists; snare classification remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_sender_coalition_formation, empirical, 'Coalition formation feasibility for legitimate senders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(email_deliverability_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(email_deliv_tr_t0, email_deliverability_bottleneck, theater_ratio, 0, 0.52).
narrative_ontology:measurement(email_deliv_tr_t5, email_deliverability_bottleneck, theater_ratio, 5, 0.6).
narrative_ontology:measurement(email_deliv_tr_t10, email_deliverability_bottleneck, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(email_deliv_be_t0, email_deliverability_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(email_deliv_be_t5, email_deliverability_bottleneck, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(email_deliv_be_t10, email_deliverability_bottleneck, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(email_deliverability_bottleneck, information_standard).
narrative_ontology:affects_constraint(email_deliverability_bottleneck, sender_reputation_asymmetry).
narrative_ontology:affects_constraint(email_deliverability_bottleneck, email_authentication_standardization).
narrative_ontology:affects_constraint(email_deliverability_bottleneck, mailbox_provider_monopoly).

% DUAL FORMULATION NOTE:
% The email deliverability bottleneck is a composite constraint family. Upstream constraints include sender reputation asymmetry (powerless senders cannot build reputation capital; institutional senders can) and mailbox provider monopoly (Gmail/Outlook control >80% of inboxes). The deliverability bottleneck emerges from the interaction of these structural constraints. Downstream constraint: email authentication standardization (whether DMARC/DKIM actually reduces the gatekeeper effect) is contingent on this bottleneck's strength.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(email_deliverability_bottleneck, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
